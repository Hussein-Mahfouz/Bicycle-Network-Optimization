Road Segment Prioritization for Bicycle Infrastructure
================
Hussein Mahfouz
18/8/2020

This repository contains the code used for the dissertation of my MSc in
[Smart Cities and Urban
Analytics](https://www.ucl.ac.uk/prospective-students/graduate/taught-degrees/smart-cities-urban-analytics-msc)
at CASA UCL. Below is an explanation of the scripts used, and how the
analysis can be reproduced.

## Paper

The paper can be found in the repo, or through this
[link](https://github.com/Hussein-Mahfouz/Bicycle-Network-Optimization/blob/master/Paper/CASA_Dissertation_16_August.pdf)

-----

## Missing Data

There are a couple of files that cannot be synced to github due to their
size. These files are neseccary for the scripts to run. Below are links
to where you can download them, and instructions on where to place them
in the repo file structure

Flow Data (2011 Census Origin-Destination Data):

  - Source: <https://www.nomisweb.co.uk/census/2011/bulk/rOD1> Choose
    File **“WU03EW”**
  - Location in Repo: data-raw/flow\_data.csv

Middle Layer Super Output Areas (December 2011) Boundaries:

  - Source:
    <http://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0>
  - Location in Repo: data-raw/MSOA\_2011\_Boundaries/\[Add files here\]

-----

## Scripts

The scripts should be run in the order they are numbered in (and listed
in here). The only exception is \_x\_dodgr\_weighting\_profiles.R.

#### \_\_*1.0\_get\_flow\_data.R*

This script matches MSOA to major towns and cities using data from
[here](http://geoportal.statistics.gov.uk/datasets/78ff27e752e44c3194617017f3f15929).
It then matches the results to the Census flow data so that all OD pairs
have an Origin City and Destination City (MSOAs in rural areas are not
matched to a city)

In this script, you choose which city you wish to run the analysis from
this list of available cities:

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jmwahgbxzv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jmwahgbxzv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jmwahgbxzv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jmwahgbxzv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jmwahgbxzv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jmwahgbxzv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jmwahgbxzv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jmwahgbxzv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jmwahgbxzv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jmwahgbxzv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jmwahgbxzv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jmwahgbxzv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jmwahgbxzv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jmwahgbxzv .gt_from_md > :first-child {
  margin-top: 0;
}

#jmwahgbxzv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jmwahgbxzv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jmwahgbxzv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jmwahgbxzv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jmwahgbxzv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jmwahgbxzv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jmwahgbxzv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jmwahgbxzv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jmwahgbxzv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jmwahgbxzv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jmwahgbxzv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jmwahgbxzv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jmwahgbxzv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jmwahgbxzv .gt_left {
  text-align: left;
}

#jmwahgbxzv .gt_center {
  text-align: center;
}

#jmwahgbxzv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jmwahgbxzv .gt_font_normal {
  font-weight: normal;
}

#jmwahgbxzv .gt_font_bold {
  font-weight: bold;
}

#jmwahgbxzv .gt_font_italic {
  font-style: italic;
}

#jmwahgbxzv .gt_super {
  font-size: 65%;
}

#jmwahgbxzv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="jmwahgbxzv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Towns &
Cities

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Barnsley

</td>

<td class="gt_row gt_left">

Basildon

</td>

<td class="gt_row gt_left">

Basingstoke

</td>

<td class="gt_row gt_left">

Bath

</td>

<td class="gt_row gt_left">

Bedford

</td>

<td class="gt_row gt_left">

Birkenhead

</td>

<td class="gt_row gt_left">

Birmingham

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Blackburn

</td>

<td class="gt_row gt_left">

Blackpool

</td>

<td class="gt_row gt_left">

Bolton

</td>

<td class="gt_row gt_left">

Bournemouth

</td>

<td class="gt_row gt_left">

Bracknell

</td>

<td class="gt_row gt_left">

Bradford

</td>

<td class="gt_row gt_left">

Brighton and Hove

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Bristol

</td>

<td class="gt_row gt_left">

Burnley

</td>

<td class="gt_row gt_left">

Burton upon Trent

</td>

<td class="gt_row gt_left">

Bury

</td>

<td class="gt_row gt_left">

Cambridge

</td>

<td class="gt_row gt_left">

Cardiff

</td>

<td class="gt_row gt_left">

Carlisle

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Chatham

</td>

<td class="gt_row gt_left">

Chelmsford

</td>

<td class="gt_row gt_left">

Cheltenham

</td>

<td class="gt_row gt_left">

Chester

</td>

<td class="gt_row gt_left">

Chesterfield

</td>

<td class="gt_row gt_left">

Colchester

</td>

<td class="gt_row gt_left">

Coventry

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Crawley

</td>

<td class="gt_row gt_left">

Darlington

</td>

<td class="gt_row gt_left">

Derby

</td>

<td class="gt_row gt_left">

Doncaster

</td>

<td class="gt_row gt_left">

Dudley

</td>

<td class="gt_row gt_left">

Eastbourne

</td>

<td class="gt_row gt_left">

Exeter

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Gateshead

</td>

<td class="gt_row gt_left">

Gillingham

</td>

<td class="gt_row gt_left">

Gloucester

</td>

<td class="gt_row gt_left">

Grimsby

</td>

<td class="gt_row gt_left">

Guildford

</td>

<td class="gt_row gt_left">

Halifax

</td>

<td class="gt_row gt_left">

Harlow

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Harrogate

</td>

<td class="gt_row gt_left">

Hartlepool

</td>

<td class="gt_row gt_left">

Hastings

</td>

<td class="gt_row gt_left">

Hemel Hempstead

</td>

<td class="gt_row gt_left">

High Wycombe

</td>

<td class="gt_row gt_left">

Huddersfield

</td>

<td class="gt_row gt_left">

Ipswich

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Kingston upon Hull

</td>

<td class="gt_row gt_left">

Leeds

</td>

<td class="gt_row gt_left">

Leicester

</td>

<td class="gt_row gt_left">

Lincoln

</td>

<td class="gt_row gt_left">

Liverpool

</td>

<td class="gt_row gt_left">

London

</td>

<td class="gt_row gt_left">

Luton

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Maidstone

</td>

<td class="gt_row gt_left">

Manchester

</td>

<td class="gt_row gt_left">

Mansfield

</td>

<td class="gt_row gt_left">

Middlesbrough

</td>

<td class="gt_row gt_left">

Milton Keynes

</td>

<td class="gt_row gt_left">

Newcastle upon Tyne

</td>

<td class="gt_row gt_left">

Newcastle-under-Lyme

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Newport

</td>

<td class="gt_row gt_left">

Northampton

</td>

<td class="gt_row gt_left">

Norwich

</td>

<td class="gt_row gt_left">

Nottingham

</td>

<td class="gt_row gt_left">

Nuneaton

</td>

<td class="gt_row gt_left">

Oldham

</td>

<td class="gt_row gt_left">

Oxford

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Peterborough

</td>

<td class="gt_row gt_left">

Plymouth

</td>

<td class="gt_row gt_left">

Poole

</td>

<td class="gt_row gt_left">

Portsmouth

</td>

<td class="gt_row gt_left">

Preston

</td>

<td class="gt_row gt_left">

Reading

</td>

<td class="gt_row gt_left">

Redditch

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Rochdale

</td>

<td class="gt_row gt_left">

Rotherham

</td>

<td class="gt_row gt_left">

Salford

</td>

<td class="gt_row gt_left">

Scunthorpe

</td>

<td class="gt_row gt_left">

Sheffield

</td>

<td class="gt_row gt_left">

Shrewsbury

</td>

<td class="gt_row gt_left">

Slough

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Solihull

</td>

<td class="gt_row gt_left">

South Shields

</td>

<td class="gt_row gt_left">

Southampton

</td>

<td class="gt_row gt_left">

Southend-on-Sea

</td>

<td class="gt_row gt_left">

Southport

</td>

<td class="gt_row gt_left">

St Albans

</td>

<td class="gt_row gt_left">

St Helens

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Stevenage

</td>

<td class="gt_row gt_left">

Stockport

</td>

<td class="gt_row gt_left">

Stockton-on-Tees

</td>

<td class="gt_row gt_left">

Stoke-on-Trent

</td>

<td class="gt_row gt_left">

Sunderland

</td>

<td class="gt_row gt_left">

Sutton Coldfield

</td>

<td class="gt_row gt_left">

Swansea

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Swindon

</td>

<td class="gt_row gt_left">

Telford

</td>

<td class="gt_row gt_left">

Wakefield

</td>

<td class="gt_row gt_left">

Walsall

</td>

<td class="gt_row gt_left">

Warrington

</td>

<td class="gt_row gt_left">

Watford

</td>

<td class="gt_row gt_left">

West Bromwich

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Weston-Super-Mare

</td>

<td class="gt_row gt_left">

Wigan

</td>

<td class="gt_row gt_left">

Woking

</td>

<td class="gt_row gt_left">

Wolverhampton

</td>

<td class="gt_row gt_left">

Worcester

</td>

<td class="gt_row gt_left">

Worthing

</td>

<td class="gt_row gt_left">

York

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

This is done in line 17. For example:

``` r
chosen_city <- "Manchester"
```

The script then filters all flow data where both the Origin MSOA **AND**
the destination MSOA are in the chosen city

If you wish to run the analysis on London, then make sure you have a
computer that is up to the task. I didn’t :(

#### \_\_*2.0\_distance\_and\_elevation.R*

This script is used to get the distance and slope between each OD pair

  - Distance: The routed distance using the
    [dodgr](https://atfutures.github.io/dodgr/) package.
  - Slope: The average slope along the route seperating the OD pair,
    using the [slopes](https://itsleeds.github.io/slopes/) package

#### \_\_*3.0\_potential\_demand.R*

This script is used to estimate where additional cycling demand will
come from. Let’s say that the target for Manchester is a 10% increase in
cyling mode share, how many additional cyclists do we assign to each OD
pair to reach that target mode share

1.  Predict Probability of Cycling Between Each OD Pair Based On
    Geography
    1.  A glm is used to predict probability of cycling based on
        distance and slope
2.  Accounting for Existing Mode Share
    1.  Look at performance of each OD pair and assign additional
        cyclists accordingly. OD pairs that have a low cycling mode
        share are allocated more cyclists than OD pairs that already
        have a high cycling mode share. This is because OD pairs with
        low cycling mode share have more potential (latent demand) than
        OD pairs with high cycling mode share
3.  Scaling Results To Match Mode Share Target
    1.  Specify target mode share increase (default is 10% but this is
        unreasonable for a city like Cambridge that already has a
        cycling mode share of 40%)
    2.  Scale potential cycling demand up or down so that it matches the
        target %
increase

#### \_\_*3.1\_plot\_mode\_shares.R* , \_\_3.2\_plot\_od\_comparison.R\_ , \_\_3.3\_plot\_desire\_lines\_current\_vs\_potential.R\_

These three scripts plot the results of \_\_*3.0\_potential\_demand.R* .

  - Compare the distance distribution of existing cycling mode share and
    potential cycling mode
share:

<p float="left">

<img src="./data/Manchester/Plots/histogram_distance_all_vs_cycling.png" width="250" />
<img src="./data/Manchester/Plots/histogram_distance_all_vs_cycling_potential.png" width="250" />
<img src="./data/Manchester/Plots/histogram_distance_cycling_potential_vs_current.png" width="250" />

</p>

  - Vizualize Existing and Potential Cycling Flow as Desire Lines

![desire lines](./data/Manchester/Plots/desire_facet_cycling.png)

  - Examine where potential cycling demand is assigned.

The methodology in \_\_*3.0\_potential\_demand.R* insures that OD pairs
that have a low cycling mode share are allocated more cyclists than OD
pairs that already have a high cycling mode share. In the figure below,
the x axis is a ratio of the cycling mode share of the OD pair to its
expected cycling mode share. The expected cycling mode share is obtained
from a glm where distance, sqrt(distance), and slope are used as
predictors. Looking at the resulting cycling mode share, we see that OD
pairs between 2-8km have the highest mode share (consistent with
bell-shaped distribution of cycling vs distance), and that mode share
increase is highest for OD pairs that have lower than expected cycling
mode
shares.

<p float="center">

<img src="./data/Manchester/Plots/mode_share_increase_vs_performance_smooth.png" width="400" />

</p>

#### \_\_*x\_dodgr\_weighting\_profiles.R*

The [dodgr](https://atfutures.github.io/dodgr/) package is used to route
the cycling demand (flow) onto the road network. This is done using
different weighting profiles, as explained in the
[documentation](https://atfutures.github.io/dodgr/reference/weighting_profiles.html)
of the package. This script is used to download a json file of the
weight profile and edit the ‘bicycle’ entries. Weights are assigned to
all OSM road types (for example, we assign a weight of 0 to make sure
that no cycling routes utilize them). The weighting profiles used are
explained in the methodology.

The weighting profiles used in the analysis are in the data file of the
repo. These are: \* weight\_profile\_unweighted.json: unweighted
shortest paths \* weight\_profile\_weighted.json: weighted shortest
paths (weighting profile explained in methodology) \*
weight\_profile\_no\_primary\_trunk.json: weighted shortest paths with
cycling banned on primary and trunk roads

These weighting profiles are used in script
\_\_*4.0\_aggregating\_flows.R*

#### \_\_*4.0\_aggregating\_flows.R*

This script uses the The [dodgr](https://atfutures.github.io/dodgr/)
package is used to route potential cycling demand onto the road network.
This is done for the different weighting profiles used in the analysis

#### \_\_*5.0\_identifying\_cycle\_infastructure\_from\_osm\_tags.R*

This script is used to identify all road segments that have segregated
cycling lanes. This includes all roads that match any of the 3 following
tags:

  - highway = cycleway  
  - cycleway = track
  - bicycle = designated

#### \_\_*6.0\_comparing\_weighting\_profiles.R*

Here we analyze the street network configuration of the city by
comparing the unweighted shortest paths to the weighted shortest paths
(check methodology for explanation of weighted shortest paths). The
aggregated flow shows us which road types are used, and it is clear that
cycleways are not utilized unless the road network is weighted to create
a hierarchy of road type preference.

![Unweighted
Routing](./data/Manchester/Plots/flows_facet_unweighted.png)

![Weighted Routing](./data/Manchester/Plots/flows_facet_weighted.png)

#### \_\_*7.0\_community\_detection.R*

Using the potential cycling demand between OD pairs, we are able to
define communities in the network. The nodes are the population-weighted
MSOA centroids (location obtained from the
[pct](https://itsleeds.github.io/pct/) package) and the links between
them are weighted by the potential cycling demand between them. The
Louvian algorithm is used to assign each MSOA centroid to a community,
and then each road segment on the network is assigned to the same
community as the MSOA centroid closest to it. The results for Manchester
are shown below

![Community
Detection](./data/Manchester/Plots/communities_alternative_Manchester.png)

#### \_\_*8.0\_growing\_a\_network.R*

This script contains all the functions for prioritizing road segments
for dedicated infrastructure. It is necessary to run this script before
8.1 and 8.2. The speed of the functions is inversely proportional to the
size of the city being analyzed (Script 8.2 takes almost 2 hours for
Birmingham on a 2.7 GHz Intel Core i5 laptop with 8GB of RAM)

#### \_\_*8.1\_plot\_network\_growth.R*

Here we obtain results for the utilitarian growth functions (Algorithms
1 and 2 in the paper)

##### Algorithm 1: Growth from One Origin

Logic:

1.  Identify link with highest flow and use it as a starting point for
    the solution
2.  Identify all links that neighbor links in the current solution
3.  Select neighboring link with highest flow and add it to the solution
4.  Repeat steps 2 & 3 until all flow is satisfied or investment
    threshold is
met

Results:

<p float="left">

<img src="./data/Manchester/Plots/Growth_Results/growth_one_seed_satisfied_km_all_flow_column.png" width="350" />
<img src="./data/Manchester/Plots/Growth_Results/growth_one_seed_satisfied_km_community_flow_column.png" width="350" />

</p>

##### Algorithm 2 (Utilitarian Growth)

Logic:

1.  Identify all links **that have dedicated cycling infrastructure**
    and add them to the initialsolution
2.  Identify all links that neighbor links in the current solution
3.  Select neighboring link with highest flow and add it to the solution
4.  Repeat steps 2 & 3 until all flow is satisfied or investment
    threshold is
met

Results:

<p float="left">

<img src="./data/Manchester/Plots/Growth_Results/growth_existing_infra_satisfied_km_all_flow_column.png" width="350" />
<img src="./data/Manchester/Plots/Growth_Results/growth_existing_infra_satisfied_km_community_flow_column.png" width="350" />

</p>

The results show the priority of each road segment (Roads are grouped
into 100km groups for vizualization
purposes)

<p float="center">

<img src="./data/Manchester/Plots/Growth_Results/growth_existing_infra_priority_all_FLOW.png" width="450" />

</p>

#### \_\_*8.2\_plot\_network\_growth\_community.R*

Here we obtain results for the egalitarian growth function (Algorithms 3
in the paper). We also compare the connectivity of the network proposed
by both algorithms

##### Algorithm 3 (Egalitarian Growth)

Logic:

1.  Identify all links that have dedicated cycling infrastructure and
    add them to the initial solution
2.  Identify all links that neighbor links in the current solution
3.  Select **from each community** one neighboring link with highest
    flow and add it to thesolution
4.  If there are no more neighboring links in a community, select the
    link with the highest flow in the community, regardless of
    connectivity, and add it to the solution
5.  Repeat steps 2, 3 & 4 until all flow is satisfied or investment
    threshold is
met

Results:

<p float="left">

<img src="./data/Manchester/Plots/Growth_Results/growth_community_4_satisfied_km_all_flow_column.png" width="350" />
<img src="./data/Manchester/Plots/Growth_Results/growth_community_4_satisfied_km_community_flow_column.png" width="350" />

</p>

Priority of each road segment, and utilization of different OSM road
types:

<p float="left">

<img src="./data/Manchester/Plots/Growth_Results/growth_community_4_priority_all_FLOW.png" width="400" />
<img src="./data/Manchester/Plots/Growth_Results/growth_community_4_investment_highways_flow.png" width="400" />

</p>

##### Comparing Connectivity of Algorithm 2 and 3

We check the number of connected components and the size of the Largest
Connected Component as road segments are added to the solution (the
components are the road segments). The initial number of components
depends on the existing bicycle network of the city. For Manchester, we
can see that the existing bicycle network has over 120 disconnected
components (Remember we are only looking at segregated bicycle
infrastructure, not painted bicycle
lanes).

<p float="left">

<img src="./data/Manchester/Plots/Growth_Results/growth_existing_infra_components_gcc_comparisonManchester.png" width="350" />
<img src="./data/Manchester/Plots/Growth_Results/growth_existing_infra_components_number_comparisonManchester.png" width="350" />

</p>

The algorithms seem to provide comparable connectivity gains.
