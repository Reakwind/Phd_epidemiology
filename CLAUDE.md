# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview
This is an R Shiny web application called "780 Data Explorer" that provides interactive data exploration capabilities for clinical/research data stored in a SQLite database. The application features patient lookup, variable distribution analysis, and bivariate analysis with interactive visualizations.

## Development Commands

### Running the Application
```r
# From R console or RStudio
shiny::runApp("app.R")

# Or from command line
Rscript -e "shiny::runApp('app.R')"
```

### Required Libraries
The application depends on these R packages:
- `shiny` - Core Shiny framework
- `DBI` & `RSQLite` - Database connectivity
- `ggplot2` - Static plotting
- `plotly` - Interactive plots
- `dplyr` - Data manipulation
- `shinythemes` - UI theming
- `shinycssloaders` - Loading animations

Install missing packages with:
```r
install.packages(c("shiny", "DBI", "RSQLite", "ggplot2", "dplyr", "shinythemes", "shinycssloaders", "plotly"))
```

## Architecture

### Data Flow
1. **Startup**: App connects to `780_database.db`, loads all data from `V_all_client_data` view into memory, then disconnects
2. **Data Processing**: Numeric conversion and quantile calculation (10th/90th percentiles) for outlier detection
3. **Runtime**: All operations work with pre-loaded data for performance

### Key Components

#### Database
- **File**: `780_database.db` (SQLite)
- **Main View**: `V_all_client_data` - contains all client data used by the application
- **Schema**: Client data with mixed numeric/categorical variables, indexed by `client_name`

#### UI Structure
- **Theme**: Yeti (professional Bootstrap theme)
- **Layout**: Sidebar with conditional inputs + main panel with 3 tabs
- **Tabs**:
  - Patient Lookup: Individual client data with filtering options
  - Variable Distribution: Single variable histograms/bar charts
  - Bivariate Analysis: Scatter plots, box plots, or jitter plots

#### Server Logic
- **Reactive Sidebar**: Changes inputs based on active tab
- **Patient Search**: Event-reactive search with outlier/missing data filtering
- **Plotting**: Uses ggplot2 + plotly for interactive visualizations
- **Data Handling**: Robust NA handling, automatic plot type selection based on data types

### Data Processing Notes
- All non-name/gender columns are converted to numeric on startup
- Quantile thresholds (10%/90%) are pre-calculated for outlier detection
- Patient lookup transposes data for vertical display
- Bivariate analysis removes incomplete observations before plotting

## Enhanced Features (September 2025)

### New Capabilities Added
1. **Enhanced Patient Search**: Autocomplete dropdown with searchable client names
2. **Data Export**: Download plots as PNG and filtered data as CSV files
3. **Statistical Summaries**: Detailed statistics for each variable (mean, median, std dev, etc.)
4. **Data Filtering**: Gender and age range filters for analysis
5. **Correlation Analysis**: Detailed correlation statistics with interpretation
6. **Data Overview Tab**: Complete dataset summary with missing data statistics
7. **Enhanced UI**: Professional styling with wellPanels and improved layout
8. **Error Handling**: Robust database connection error handling and input validation

### Current Files
- `app.R` - Main enhanced application (recommended version with all features)
- `app_basic.R` - Basic version backup (fallback if needed)

### Additional Dependencies for Enhanced Version
```r
install.packages(c("DT", "corrplot", "tidyr"))
```

### New Tabs and Features
- **Variable Distribution**: Now includes summary statistics panel and export options
- **Bivariate Analysis**: Added correlation information and statistical interpretation
- **Data Overview**: New tab showing complete dataset summary with missing data analysis
- **Enhanced Filtering**: Gender and age range filters applied across analyses

### Usage Recommendations
- Use `app.R` as the main application (enhanced version with all features)
- Use `app_basic.R` only as a fallback if the main version has issues
- The enhanced version includes autocomplete search, statistical summaries, and data export
- All visualizations are now downloadable as high-resolution PNG files
- Filtered datasets can be exported as CSV files for further analysis

## File Structure
- `app.R` - Main enhanced Shiny application (current version)
- `app_basic.R` - Basic version backup
- `780_database.db` - SQLite database with clinical data
- `780_database.sqbpro` - SQLiteStudio project file
- `rsconnect/` - Deployment configuration for shinyapps.io
- `.gitignore` - Git ignore file for R/Shiny projects