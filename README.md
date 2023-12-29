# Fauna Analysis & Report Automation App

This Shiny app is developed to support surveyors in their work. It focuses on creating in-depth reports from survey data gathered on various fauna such as amphibians, birds, mammals, and reptiles. The app is tailored for use with transect or plot survey methods and places a strong emphasis on data quality, facilitated by thorough validation processes.

## Features

- **Data Validation and Quality Control**
- **Automated Rapid Analysis** 
- **Automated Report Generation**

## Screenshots

![Screenshot 1](screenshots/screenshot1.png)
![Screenshot 2](screenshots/screenshot2.png)
![Screenshot 3](screenshots/screenshot3.png)

## Usage

### Data Validation

The app validate data by thoroughly checking the format for each column. This ensures data accuracy and adherence to predefined standards and formats. 

Key aspects of the validation process include:

1. **Format Verification:** The app rigorously inspects each column of the input data to confirm that the format aligns with the expected standards. This includes checks for data types, date formats, numerical values, and text string structures to ensure consistency and accuracy.

2. **Species Name Matching:** An integral part of the validation is the cross-referencing of submitted species names with authoritative databases. This step verifies the accuracy of species identification and nomenclature. The app utilizes specific databases for different fauna groups:
    - **Mammals:** The app cross-references mammal species names with the database found at [www.departments.bucknell.edu/biology/resources/msw3](http://www.departments.bucknell.edu/biology/resources/msw3), ensuring up-to-date and accurate mammalian taxonomy.
    - **Birds:** Bird species names are validated against the taxonomy provided by eBird at [https://ebird.org/science/use-ebird-data/the-ebird-taxonomy](https://ebird.org/science/use-ebird-data/the-ebird-taxonomy).
    - **Amphibians:** The app refers to [https://amphibiansoftheworld.amnh.org](https://amphibiansoftheworld.amnh.org) for validating amphibian species.
    - **Reptiles:** For reptiles, species validation is done using the database available at [http://www.reptile-database.org](http://www.reptile-database.org).

### Rapid Analysis

Once the data has been validated, users can swiftly obtain analyses regarding species richness and community composition from their surveys. The app offers functionalities to create and display a variety of tables and plots, reflecting the diverse aspects of the collected data. These visual representations and data tables can be independently saved for further use or reference. This feature of rapid analysis not only saves time but also provides immediate insights into the ecological aspect of the surveyed area, making it an invaluable tool for quick decision-making and preliminary data interpretation.

### Generating Reports

Following the production of data analyses, the app aids in creating detailed reports. These reports efficiently summarize the findings of the survey in a structured format. Designed for versatility, the app allows users to export these summaries in formats compatible with external editing tools, like Microsoft Word. This functionality facilitates users in adding their narrative or detailed commentary to the reports.

## Contributors

- **Achmad Alifianto**
- **Azka Razendra Zahran**
- **Erlangga Muhammad**
- **Farhan Adhyn**
- **Satya Rizki Darmawan**
- **Zana Pandya Pratisara**

And heartfelt thanks to all the colleagues at OryxGaming. Our "Activities" have led to fruitful discussions and invaluable insights.

