# Joint state assignments and reusable county matching for compulsory schooling.
msp_design <- function(slots, counts, k=30L) {
  states <- sort(unique(counts$state_id))
  slots <- data.table::copy(slots)
  slots[, options := vapply(g,function(gg) sum(counts$g == gg),integer(1))]
  data.table::setorder(slots,options,g,state_id)
  options <- lapply(slots$g,function(gg) match(counts[g == gg,state_id],states))
  years <- sort(unique(slots$g))
  N <- matrix(0,length(years),length(states))
  N[cbind(match(counts$g,years),match(counts$state_id,states))] <- counts$counties
  memo <- new.env(parent=emptyenv())
  enough <- function(mask) all(rowSums(N[,bitwAnd(mask,bitwShiftL(1L,seq_along(states)-1L)) == 0L,drop=FALSE]) >= k)
  ways <- function(i,mask) {
    key <- paste(i,mask)
    if (exists(key,memo,inherits=FALSE)) return(get(key,memo))
    n <- if (!enough(mask)) 0 else if (i > nrow(slots)) 1 else {
      sum(vapply(options[[i]],function(j) {
        bit <- bitwShiftL(1L,j-1L)
        if (bitwAnd(mask,bit)) 0 else ways(i+1L,bitwOr(mask,bit))
      },numeric(1)))
    }
    assign(key,n,memo); n
  }
  total <- ways(1L,0L)
  unrank <- function(rank) {
    stopifnot(rank >= 1, rank <= total)
    mask <- 0L; selected <- integer(nrow(slots))
    for (i in seq_len(nrow(slots))) {
      for (j in options[[i]]) {
        bit <- bitwShiftL(1L,j-1L)
        if (bitwAnd(mask,bit)) next
        n <- ways(i+1L,bitwOr(mask,bit))
        if (rank > n) {rank <- rank-n; next}
        selected[i] <- states[j]; mask <- bitwOr(mask,bit); break
      }
    }
    z <- data.table::copy(slots)[,options := NULL]
    z[, placebo_state := selected]
    z[, excluded_states := paste(sort(selected),collapse="-")]
    z
  }
  list(total=total,unrank=unrank,slots=slots,states=states)
}

msp_context <- function(features, excluded, specification) {
  columns <- if (specification == "richer") c(ms_base_features,ms_rich_features) else ms_attribute_features
  complete <- ms_complete(features,columns)
  donor_ids <- features[donor_eligible & complete & !state_id %in% excluded,id]
  target_ids <- features[donor_eligible & complete & state_id %in% excluded,id]
  # Every target state is already excluded, so all targets share one scaling
  # pool. Compute it once; preserve ms_match's exact distance and tie rules.
  donor <- features[match(donor_ids,id)]
  dx <- as.matrix(donor[,..columns])
  mu <- colMeans(dx);sigma <- apply(dx,2L,stats::sd)
  constant <- !is.finite(sigma) | sigma <= 1e-12
  lower <- apply(dx,2L,min);upper <- apply(dx,2L,max)
  tx_all <- as.matrix(features[match(target_ids,id),..columns])
  matches <- lapply(seq_along(target_ids),function(i) {
    tx <- tx_all[i,]
    empty <- function(status) list(ok=FALSE,status=status,selected=data.table::data.table(),balance=data.table::data.table())
    if(nrow(dx)<30L) return(empty("insufficient_donors"))
    unsupported <- constant & abs(tx-mu)>1e-10*pmax(1,abs(mu))
    if(any(unsupported)) return(empty(paste0("constant_donor_mismatch:",paste(columns[unsupported],collapse=";"))))
    z <- matrix(0,nrow(dx),ncol(dx))
    if(any(!constant)) z[,!constant] <- sweep(sweep(dx[,!constant,drop=FALSE],2L,tx[!constant],"-"),2L,sigma[!constant],"/")
    distance <- sqrt(rowSums(z^2)); chosen <- order(distance,donor$id)[seq_len(30L)]
    selected <- data.table::data.table(id=donor$id[chosen],state_id=donor$state_id[chosen],distance=distance[chosen])
    balance <- data.table::data.table(variable=columns,treated=as.numeric(tx),eligible_mean=mu,eligible_sd=sigma,
      selected_mean=colMeans(dx[chosen,,drop=FALSE]),eligible_min=lower,eligible_max=upper,constant=constant)
    balance[,outside_range:=treated<eligible_min | treated>eligible_max]
    balance[,selected_std_gap:=ifelse(constant,0,(treated-selected_mean)/eligible_sd)]
    list(ok=TRUE,status="matched",selected=selected,balance=balance)
  })
  names(matches) <- as.character(target_ids)
  status <- data.table::data.table(id=target_ids,state_id=features[match(target_ids,id),state_id],
    status=vapply(matches,`[[`,character(1),"status"))
  list(excluded=excluded,columns=columns,donor_ids=donor_ids,status=status,matches=matches)
}

msp_pool <- function(states, slots, weight_column) {
  z <- merge(states,slots[,.(state_id,weight=get(weight_column))],by="state_id")
  stopifnot(all(abs(z[,.(total=sum(weight)),by=event_time]$total-1) < 1e-10))
  z[,.(treated=sum(weight*treated),synthetic=sum(weight*synthetic),
       gap=sum(weight*gap),gap_adjusted=sum(weight*gap_adjusted)),by=event_time][order(event_time)]
}

msp_check_path <- function(fit, Y, id, years, g) {
  w <- fit$weights
  stopifnot(nrow(w)==30L,!anyDuplicated(w$id),all(is.finite(w$weight)),
    min(w$weight)>=-1e-10,abs(sum(w$weight)-1)<1e-8)
  treated <- as.numeric(Y[as.character(id),])
  synthetic <- as.numeric(crossprod(w$weight,Y[as.character(w$id),,drop=FALSE]))
  gap <- treated-synthetic
  stopifnot(identical(as.integer(fit$path$year),as.integer(years)),
    max(abs(fit$path$treated-treated))<1e-10,
    max(abs(fit$path$synthetic-synthetic))<1e-10,
    max(abs(fit$path$gap-gap))<1e-10,
    max(abs(fit$path$gap_adjusted-(gap-gap[years==g-15L])))<1e-10,
    abs(fit$summary$pre_rmspe-sqrt(mean(gap[years<=g-15L]^2)))<1e-10)
  invisible(TRUE)
}
