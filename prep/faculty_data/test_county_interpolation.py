import copy,csv,tempfile,unittest
from decimal import Decimal
from fractions import Fraction
from pathlib import Path
from interpolate_county_faculty import NEW_FIELDS,count,interpolate_row,write_csv
from validate_county_interpolation import validate_export


def row(year,value='',geoid='01001',fte=''):
    return dict(GEOID=geoid,year=str(year),faculty_headcount=value,faculty_fte=fte,
                coverage_status='observed_source_sum_partial_universe' if value else 'unobserved_not_zero')


class CountyInterpolation(unittest.TestCase):
    def test_unequal_year_spacing(self):
        anchors=[(1900,'10'),(1903,'19'),(1910,'26')]
        a=interpolate_row(row(1901),anchors);b=interpolate_row(row(1905),anchors)
        self.assertEqual(a['faculty_headcount_annual'],'13')
        self.assertEqual(b['faculty_headcount_annual'],'21')
        self.assertEqual(b['faculty_headcount_gap_years'],'7')

    def test_fractional_estimates_not_integer_rounded(self):
        result=interpolate_row(row(1901),[(1900,'0'),(1903,'1')])
        self.assertGreater(Fraction(result['faculty_headcount_annual']),0)
        self.assertLess(abs(Fraction(result['faculty_headcount_annual'])-Fraction(1,3)),Fraction(1,10**48))

    def test_reported_zero_is_valid_anchor(self):
        result=interpolate_row(row(1901),[(1900,'0'),(1902,'8')])
        self.assertEqual(result['faculty_headcount_annual'],'4')
        self.assertEqual(result['faculty_headcount_left_value'],'0')
        self.assertEqual(interpolate_row(row(1901),[(1900,'0'),(1902,'0')])['faculty_headcount_annual'],'0')

    def test_observed_representation_exact_and_input_not_mutated(self):
        original=row(1900,'10.000');before=copy.deepcopy(original)
        result=interpolate_row(original,[(1900,'10.000'),(1910,'20')])
        self.assertEqual(result['faculty_headcount_annual'],'10.000')
        self.assertEqual(result['faculty_headcount_status'],'observed')
        self.assertEqual(original,before)
        self.assertEqual(result['faculty_headcount_left_year'],'')

    def test_missing_edges_zero_and_one_observation(self):
        for anchors,year in [([],1900),([(1900,'4')],1901),([(1900,'4')],1899),
                             ([(1900,'4'),(1910,'8')],1899),([(1900,'4'),(1910,'8')],1911)]:
            result=interpolate_row(row(year),anchors)
            self.assertEqual(result['faculty_headcount_status'],'missing')
            self.assertEqual(result['faculty_headcount_annual'],'')

    def test_long_gap_has_no_cap(self):
        result=interpolate_row(row(1900),[(1852,'2'),(1939,'89')])
        self.assertEqual(result['faculty_headcount_annual'],'50')
        self.assertEqual(result['faculty_headcount_gap_years'],'87')

    def test_FTE_and_coverage_unchanged(self):
        original=row(1901,fte='12.5');result=interpolate_row(original,[(1900,'0'),(1902,'2')])
        self.assertTrue(all(result[key]==value for key,value in original.items()))
        self.assertEqual(result['faculty_fte'],'12.5')
        self.assertEqual(result['coverage_status'],'unobserved_not_zero')

    def test_nonfinite_or_negative_anchor_rejected(self):
        for value in ['NaN','Infinity','-1']:
            with self.assertRaises(ValueError): count(value)

    def test_two_county_export_and_tampering(self):
        original=[row(year,'0' if year==1900 else '3' if year==1903 else '') for year in range(1900,1904)]
        original += [row(year,'100' if year==1902 else '',geoid='01003') for year in range(1900,1904)]
        points={'01001':[(1900,'0'),(1903,'3')],'01003':[(1902,'100')]}
        result=[interpolate_row(r,points[r['GEOID']]) for r in original]
        with tempfile.TemporaryDirectory() as directory:
            source=Path(directory)/'observed.csv';target=Path(directory)/'annual.csv'
            write_csv(source,original,list(original[0]));write_csv(target,result,list(original[0])+NEW_FIELDS)
            report=validate_export(source,target)
            self.assertEqual(report['interpolated_cells'],2)
            self.assertEqual(report['remaining_missing_cells'],3)
            result[1]['faculty_headcount_annual']='99'
            write_csv(target,result,list(original[0])+NEW_FIELDS)
            with self.assertRaises(AssertionError): validate_export(source,target)
            result[1]=interpolate_row(original[1],points['01001']);result[1]['faculty_fte']='1'
            write_csv(target,result,list(original[0])+NEW_FIELDS)
            with self.assertRaises(AssertionError): validate_export(source,target)


if __name__=='__main__': unittest.main()
