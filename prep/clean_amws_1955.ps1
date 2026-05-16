# Clean OCR'd birthplace strings from 1955 American Men of Science
param(
    [string]$InputFile = 'C:/Users/deivi/github/elite-schools/prep/output/amws_1955_batches/in/00136.tsv',
    [string]$OutputFile = 'C:/Users/deivi/github/elite-schools/prep/output/amws_1955_batches/out/00136.jsonl'
)

# State abbreviation mappings (case-sensitive lookup with fallback to case-insensitive)
$stateMap = @{
    'N. Y' = 'NY'; 'N.Y' = 'NY'; 'NY' = 'NY'; 'N. Y.' = 'NY'
    'MASS' = 'MA'; 'Mass' = 'MA'; 'Maas' = 'MA'; 'MA' = 'MA'
    'PA' = 'PA'; 'Pa' = 'PA'; 'p' = 'PA'
    'CALIF' = 'CA'; 'Calif' = 'CA'; 'CA' = 'CA'
    'ILL' = 'IL'; 'Ill' = 'IL'; 'M' = 'IL'; '111' = 'IL'; 'IL' = 'IL'
    'CONN' = 'CT'; 'Conn' = 'CT'; 'CT' = 'CT'
    'NEBR' = 'NE'; 'Nebr' = 'NE'; 'NE' = 'NE'
    'IND' = 'IN'; 'Ind' = 'IN'; 'IN' = 'IN'
    'WASH' = 'WA'; 'Wash' = 'WA'; 'WA' = 'WA'
    'MICH' = 'MI'; 'Mich' = 'MI'; 'MI' = 'MI'
    'N MEX' = 'NM'; 'N. Mex' = 'NM'; 'N Mex' = 'NM'; 'NM' = 'NM'
    'ONT' = ''; 'Ont' = ''; 'O. N. T' = ''; 'O.N.T.' = ''; 'Ontario' = ''
    'DEL' = 'DE'; 'Del' = 'DE'; 'DE' = 'DE'
    'KY' = 'KY'; 'Ky' = 'KY'; 'KENTUCKY' = 'KY'; 'Kentucky' = 'KY'
    'MAINE' = 'ME'; 'Maine' = 'ME'; 'ME' = 'ME'
    'KANS' = 'KS'; 'Kans' = 'KS'; 'KS' = 'KS'
    'COLO' = 'CO'; 'Colo' = 'CO'; 'CO' = 'CO'
    'MINN' = 'MN'; 'Minn' = 'MN'; 'MN' = 'MN'
    'OREGON' = 'OR'; 'Oregon' = 'OR'; 'OR' = 'OR'
    'OHIO' = 'OH'; 'Ohio' = 'OH'; 'OH' = 'OH'
    'W. VA' = 'WV'; 'W Va' = 'WV'; 'W.Va' = 'WV'; 'WV' = 'WV'
    'D. C' = ''; 'D.C' = ''; 'D.C.' = ''; 'DC' = ''
    'H. I' = 'HI'; 'HI' = 'HI'; 'H. I.' = 'HI'
    'CAN' = ''; 'Can' = ''; 'CANADA' = ''; 'Canada' = ''
    'N. B, Can' = ''; 'N B Can' = ''; 'N. B' = ''
    'P. I' = ''; 'Philippines' = ''; 'P.I.' = ''
}

# Read input file
$lines = Get-Content $InputFile | Select-Object -Skip 1

$results = @()
$rowCount = 0

foreach ($line in $lines) {
    if ([string]::IsNullOrWhiteSpace($line)) { continue }

    $rowCount++

    $parts = $line -split "`t"
    if ($parts.Count -lt 2) { continue }

    $lineid = [int]$parts[0]
    $birthplace = $parts[1]

    # Initialize output object
    $obj = [ordered]@{
        lineid = $lineid
        city = ""
        state = ""
        country = ""
        date = ""
        nat = ""
        flag = ""
    }

    # Extract date first (stops at first date pattern)
    $datePattern = '([A-Za-z]+\.?\s+\d{1,2},?\s*\d{2}|\w+\s+\d{1,2},\s*\d{2})'
    $dateMatch = [regex]::Match($birthplace, $datePattern)

    $birthplaceForParsing = $birthplace

    if ($dateMatch.Success) {
        $obj.date = $dateMatch.Value.Trim()
        $birthplaceForParsing = $birthplace.Substring(0, $dateMatch.Index).Trim()
    }

    # Extract nat info from original (can appear after date)
    $natMatch = [regex]::Match($birthplace, 'nat\.?\s*(\d{2})?')
    if ($natMatch.Success) {
        if ($natMatch.Groups[1].Success) {
            $obj.nat = "nat. " + $natMatch.Groups[1].Value
        } else {
            $obj.nat = "nat"
        }
    }

    # Clean birthplace string
    $birthplaceForParsing = $birthplaceForParsing -replace '^\s*[@&\s]+', ''
    $birthplaceForParsing = $birthplaceForParsing -replace '\s*[-;,.\s]*$', ''

    # Rejoin hyphen-broken words (e.g., "Mar- tlnsburg" -> "Martinsburg")
    $birthplaceForParsing = [regex]::Replace($birthplaceForParsing, '(\w+)-\s+(\w+)', '$1$2')

    # Split by comma to get city, state, country parts
    $parts = @($birthplaceForParsing -split ',\s*' | ForEach-Object { $_.Trim() } | Where-Object { $_ })

    if ($parts.Count -gt 0) {
        $obj.city = $parts[0]

        # Determine state and country
        if ($parts.Count -eq 1) {
            # Just city - assume USA
            $obj.state = ""
            $obj.country = "USA"
        } elseif ($parts.Count -eq 2) {
            $stateOrCountry = $parts[1]

            # Check if it's a known country designation
            if ($stateOrCountry -match '(?:Can|Ont|Ont\.|O\.N\.T\.|N\.?\s*B)') {
                $obj.state = ""
                $obj.country = "Canada"
            } elseif ($stateOrCountry -match 'P\.?\s*I') {
                $obj.state = ""
                $obj.country = "Philippines"
            } else {
                # Check state map
                if ($stateMap.ContainsKey($stateOrCountry)) {
                    $mapped = $stateMap[$stateOrCountry]
                    if ($mapped -eq "") {
                        # This was a country abbreviation
                        $obj.state = ""
                        $obj.country = "Canada"
                    } else {
                        $obj.state = $mapped
                        $obj.country = "USA"
                    }
                } else {
                    # Treat as state abbreviation
                    $obj.state = $stateOrCountry
                    $obj.country = "USA"
                }
            }
        } elseif ($parts.Count -eq 3) {
            # City, intermediate, country/state
            $stateOrCountry = $parts[1]
            $final = $parts[2]

            # Check if final part is Canada indicator
            if ($final -match '(?:Can|Ont)') {
                $obj.state = ""
                $obj.country = "Canada"
            } else {
                # Use second part as state
                if ($stateMap.ContainsKey($stateOrCountry)) {
                    $mapped = $stateMap[$stateOrCountry]
                    $obj.state = if ($mapped -eq "") { "" } else { $mapped }
                } else {
                    $obj.state = $stateOrCountry
                }
                $obj.country = "USA"
            }
        }
    }

    # Set flags
    if (-not $obj.date) {
        $obj.flag = "no_date"
    }

    # Convert to JSON
    $json = $obj | ConvertTo-Json -Compress
    $results += $json
}

# Write output
$results | Out-File -FilePath $OutputFile -Encoding UTF8 -NoNewline

# Add newlines between JSON objects
$content = Get-Content $OutputFile
$content -join "`n" | Out-File -FilePath $OutputFile -Encoding UTF8

Write-Host "Processed $rowCount rows"
$rowCount
