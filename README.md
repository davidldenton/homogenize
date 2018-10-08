## Data Homogenization Machine

This app will homogenize one or more variables for each value in a given ID column.

Data is rarely clean (homogenous) enough for use in modeling, reporting, or dashboarding. Null data, human error, and the presence of multiple accurate values can make it difficult to build a good data product or perform a useful analysis. A customer table, for instance, may have multiple correct entires for phone number (mobile, office, home, etc.). It could also have missing and inaccurate values for address if the customer has more than one residence, purchases an item as a gift, or uses different mailing and billing addresses.

In some cases, it may suit the analyst to homogenize these variables by populating them with the most commonly occurring variable for a given ID. Even though some values will be lost through homogenization, one may not need every address value that the customer has ever entered. An analysis or model may require a single address value for each customer ID (in the case where two values have equal counts, the tie is resolved alphabetically). 

The shiny application linked below will perform this data cleaning task on most common text files.

### Instructions
1. Choose a text file for homogenization.
	1) Any comma, pipe, or tab delimited text file should work.
2. Indicate whether or not a header (column names) exist in the file.
3. Choose the appropriate delimiter.
4. If the file has a header, enter the name of the ID column. If there is no header, enter the column position as a numeric value.
5. Enter the columns that require homogenization. Again, if the file has a header, enter the column name, if it does not, enter the column position.
6. Click “Submit.”
7. Homogenized data will be printed to the screen.
	1) The data will retain all variables in the original file, even those not selected for homogenization.
8. Click “Download” if the results appear satisfactory and the homogenized data will be returned as a csv file.


* Link to application: https://davidldenton.shinyapps.io/homogenize/