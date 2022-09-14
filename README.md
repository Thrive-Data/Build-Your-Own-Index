# Build Your Own Index

## What this code does?
This repository is for building a customizable index using publicly available data.

Code will be able to create an app like the one [here]("https://dkro23.shinyapps.io/byo_index_v3/").

## Why is this useful?
Organizations have to consider several factors when they decide to allocate resources given that they have limited time and resources. But what factors should an organization consider? There are several buckets of factors that typically come to mind: organizational mission (who do we say we serve), specifications of the grant (e.g. limiting the effect of learning loss in COVID impacted communities), and other factors we believe influence outcomes  (e.g. community resources). This tool will help organizations consider the complex combination of factors by allowing them to pick and choose the indicators they want to include in an index. It also allows users to add weights to the indicators because some indicators are more important to consider than others depending on the organization, grant, and community context.

## Who is this useful for?
This repository is likely most useful to data analysts working in the nonprofit space in Chicago in terms of creating a similar index tool, though I hope this can be a resource for data analysts in other cities by giving them a framework for creating similar indices from public data sources in their city. But the index itself should also be useful for a non-technical audience, including project managers and leadership in nonprofit organizations and other system leaders. 

## Data Source and Definitions
Data are from a variety of publically available sources:
- Percent Black/Hispanic Population: 2019 1-year American Community Survey, US Census
- Number of Partners: Thrive Chicago proprietary Partner Directory")
- Percent of Families with Children below Poverty Level: 2019 1-year American Community Survey, US Census
- Number of Violent Crimes: Chicago Data Portal (2021 crimes only).
- Community Disconnection Rate: Percent of 16-24 year old youth that are not in school or working. 2019 1-year American Community Survey, US Census/IPUMS.
- High Graduation Rate: HS Graduation Rate from Chicago Public Schools in Spring 2020. [To&Through](https://toandthrough.uchicago.edu/tool/cps/).
- COVID Mortality Rate: Chicago Data Portal.
- Percent of Households without Internet Access: 2019 1-year American Community Survey, US Census.
- Percent of People without Health Insurance: 2019 1-year American Community Survey, US Census.
- Percent of Adults with Poor Mental Health: 500 Cities Project, CDC. (Data current as of 2017).
- Life Expentency: Chicago Health Atlas

With the exception of "Number of Partners" (which is sourced from a proprietary data source), all other indicators are from publicly available data sources. 

The community area shapefile can be on the [Chicago Data Portal](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6).

# About

Thrive Chicago is a dedicated group of individuals who are committed to collaborative work, creative problem solving, and breaking down systemic barriers to achieve equity for young people and communities. More information about us and our work can be found [here](https://thrivechi.org/).

Questions about the code or other data-related inquiries can be sent to David Krosin at dkrosin@thrivechi.org.
