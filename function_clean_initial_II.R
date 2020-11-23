#The following code is divided into two sections:

#1) Programs aggregating data into reportable categories after cleaning in the markdown report
# (Note that this is finally applied in the report() function for all variables)
#2) In-line programs that print key phrases depending on the result

#############################
# DATA AGGREGATION (Part 1) #
#############################

# m.prep is used to read whether a date observation falls between two dates

# "rep_date" is a MMDDYYYY variable representing the month of data reported by the entry technician. 

# An initial IF statement determines if "rep_date" falls after the last day before the first 
# month, and (&&) before the first day of the following month. If the condition is met then an 
# integer will be returned representing the number of months since the intervention began. In the 
# first IF statement, for example, "rep_date" will return 0 if the reported date is listed as 
# 04/05/2021, as it falls within the first range. The mdy() lubridate function us used to read the 
# variable as a date. 

m.prep <- function(fac) {
  if ((mdy("07/31/2020") < mdy(fac["rep_date"])) &
           (mdy(fac["rep_date"]) < mdy("09/01/2020"))) {
    return(0)
  }
    #determines if "rep_date" falls after the last day of the prior month
  else if ((mdy("08/31/2020") < mdy(fac["rep_date"])) &
    #determines if "rep_date" falls before the first day of the upcoming month
      (mdy(fac["rep_date"]) < mdy("10/01/2020"))) { 
    
    return(1) #Returns days since the intervention began (this date range can vary)
    
  } else if ((mdy("09/30/2020") < mdy(fac["rep_date"])) & 
      (mdy(fac["rep_date"]) < mdy("11/01/2020"))) {
    return(2)
  } else if ((mdy("10/31/2020") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("12/01/2020"))) {
    return(3)
  } else if ((mdy("11/30/2020") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("01/01/2021"))) {
    return(4)
  } else if ((mdy("12/31/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("02/01/2021"))) {
    return(5)
  } else if ((mdy("01/31/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("03/01/2021"))) {
    return(6)
  } else if ((mdy("02/28/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("04/01/2021"))) {
    return(7)
  } else if ((mdy("03/31/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("05/01/2021"))) {
    return(8)
  } else if ((mdy("04/30/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("06/01/2021"))) {
    return(9)
  } else if ((mdy("05/31/2021") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("07/01/2021"))) {
    return(10)
  } else if ((mdy("06/30/2020") < mdy(fac["rep_date"])) &  
             (mdy(fac["rep_date"]) < mdy("08/01/2021"))) {
    return(11)
  } else if (is.na(mdy(fac["rep_date"]))) {
    return(NA)
  }
}

# MONTHS SINCE DAYS OF INTERVENTION 
# Month_Int applies m.prep (see above) to read and report observations by row. Results are returned as
# a matrix and printed in the final readable report() function at the end of this section (part 1). 

Month_Int <- function(fac) {
  data.frame(matrix(apply(fac, 1, m.prep)))
}

#TOTAL URINE CULTURE ORDERS
order_cnt <- function(fac) {
  rowSums(fac[, grep('cult_numb', names(fac))], na.rm=T)
}

# RESIDENT DAYS
# This function returns a column with the total number of resident days in a month. 
# The grep() function is used to search for all columns named "resident_days". The dataset 
# "fac" is subset out by brackets. Within REDCap, Entry Technicians may be returning to survey.
# In this event, NA value will occur as they will SKIP the question "How many total resident
# days are reported?". Therefore, an NA value will exist in this row for the collected data,
# and na.rm = T. The rowSums() function is applied to the results to replace NA values with 0. 
# Note there is only one 'resident_days' column in the dataset.

Res_Days <- function(fac) {
  rowSums(fac[, grep('resident_days', names(fac))], na.rm=T)
}

#URINE CULTURES AVAILABLE
# Returns a column with the number of urine culture and sensitivity tests ordered in a month. 
# Similar to the Res_Days function (above), all rows are summed with rowSums(), grep() subsets 
# out the 'orgavail' name, and NAs are removed. 

Urine_CS <- function(fac) {
  rowSums(fac[, grep('orgavail', names(fac))], na.rm=T)
}

# URINE CULTURES WITH >100K CFUS
# Returns a column with the number of urine cultures with greater than 100,000 colony forming 
# units (CFUs) in each row. First, the dataframe 'urin' is created to subset out all variable 
# names starting with org and including cnt; \\d is included between these two strings to capture 
# all columns with the two identifiers. A total of 108 columns are included in 'urin'. Each four columns 
# in 'urin' contains a numeric from 0-3 representing a urine lab culture result. 0 == no lab result, 
# 1 == <50,000 CFUs, 2 == 50,000 - 100,000 CFUs, 3 == >100,000 CFUs. In each row, every 4 columns 
# are lab results for the same patient. For example, row 2 columns 5-8 represents 4 results for 
# the same patient. The purpose of the urine_pos function is to identify whether any one of these 
# 4 patient cultures reported >100,000 CFUs (represented as a 3). 

# To count these data by chunks of 4 a loop is applied. In step 2, the container 'result' is created. 
# A for loop then reads each row (which represents 27 total patients) by chunks of 4 columns,
# which will count whether any one of a patient's 4 lab results included 3. The IF statement will print
# the numeric 1 (yes) if any of every 4 columns in each row contains a 3. rowSums then adds all the 1's 
# into a single column count. 

urine_pos <- function(fac) {
    #Step 1: Subset desired values into dataframe
  urin <- fac[, grep('org\\dcnt', names(fac))]
    #Step 2: Prepare a container with all 0's
  result <- matrix(data = 0, nrow = nrow(urin), ncol = (ncol(urin)/4))
    #Step 3: Loop to print whether 3 exists by chunk
  for (j in 1:nrow(urin)) {
    for (i in (1:((ncol(urin))/4))) {
        #Assess if each chunk of 4 contains a 3
      if (any(urin[j,((i*4)-3):(i*4)] == 3, na.rm = T)) {
          #If condition is met, insert a 1 in the 'result' matrix
        result[j,i] <- 1
          #If condition is not met, result position will stay as 0
      } 
    }
  }
    #Step 4: Sum rows of results to return a vector
  rowSums(result)
}

# URINE CULTURES WITH 1--99K CFUS
# Returns a column with the number of urine cultures between 0 to 100,000 CFUs. This function is 
# identical to the urine_pos function above, except the loop will print 1 to the 'result' when either 
# a 1 or 2 exists. First 'urin' is created to include all lab results for patients. The container 'result' 
# is created. A loop prints 1 to 'result' if any 4 columns for each patient contained a 1 (<50,000 CFUs) 
# or 2 (50,000 - 100,000 CFUs). rowSums then adds each 1 into a column. 

urine_poslow <- function(fac) {
  urin <- fac[, grep('org\\dcnt', names(fac))]
  result <- matrix(data = 0, nrow = nrow(urin), ncol = (ncol(urin)/4))
  #Loop to print whether 3 exists by chunk
  for (j in 1:nrow(urin)) {
    for (i in (1:((ncol(urin))/4))) {
      if (any(urin[j,((i*4)-3):(i*4)] == 1, na.rm = T)) {
        result[j,i] <- 1
      } else if (any(urin[j,((i*4)-3):(i*4)] == 2, na.rm = T)) {
        result[j,i] <- 1
      }
    }
  }
  #Return result
  rowSums(result)
}

# URINE CULTURES WITH ANY COLONY COUNT
# This function returns a vector column if any colony count was reported for a patient in the dataset. 
# This function is an extension of the two functions above by printing 1 to the 'result' container if 
# a numeric 1, 2, or 3 is noted in the dataset. As before, the numeric 0 == no lab result, 1 == <50,000 
# CFUs, 2 == 50,000 - 100,000 CFUs, 3== >100,000 CFUs. The dataframe 'urin' is first created to subset 
# out all lab results. The lab result variable names are subset out by grep and include the strings 
# org and cnt; \\d is included between these two strings as a numeric distinguishes different variable 
# names. In each row, every 4 columns are lab results for the same patient.

# A 'result' container is similarly created. The for loop reads 'urin' by row by chunks of 4. If 
# statements print 1 to the 'result' container if 1, 2, or 3 are observed in the chunk of 4. rowSums 
#sums all 1's in the 'result' row into a single vector column.

urine_posany <- function(fac) {
  urin <- fac[, grep('org\\dcnt', names(fac))]
  result <- matrix(data = 0, nrow = nrow(urin), ncol = (ncol(urin)/4))
  for (j in 1:nrow(urin)) {
    for (i in (1:((ncol(urin))/4))) {
      if (any(urin[j,((i*4)-3):(i*4)] == 1, na.rm = T)) {
        result[j,i] <- 1
      } else if (any(urin[j,((i*4)-3):(i*4)] == 2, na.rm = T)) {
        result[j,i] <- 1
      } else if (any(urin[j,((i*4)-3):(i*4)] == 3, na.rm = T)) {
        result[j,i] <- 1
      }
    }
  }
  rowSums(result)
}

# ANTIBIOTICS PRESCRIBED PRIOR TO LAB RESULT AVAILABILITY 
# This function returns a vector of the total number of antibiotics prescribed for a UTI prior to 
# availability of lab results. The purpose of this vector is to summarize whether the date reported 
# in 'repp' (which represents the date when a urine culture result was reported) falls after dates in 
# 'antt' (which represents dates at which 3 potential antibiotics were prescribed). First the dataframe 
# 'rep' is created by subsetting out all columns including the string 'repdate'. The dataset 'repp' 
# then applies the mdy() function to convert the dates into a readable date format. The process is 
# repeated for antibiotic prescription dates with 'ant' and applying mdy() for the 'antt' dataset. 
# Note that 'antt' has three times as many columns, as three potential antibiotic prescription fields 
# are available for each 'repp' field. 

# A 'result' container is established. A for loop then determines if each 'repp' observation is greater 
# than the three corresponding 'antt' observations. If the condition is met, 1 is printed to the 
# 'result' container. rowSums sums all 1's in the 'result' row into a single vector column.

uti_emp <- function(fac) {
    #Subset report date observations and convert it to readable date with ymd
  rep <- fac[, grep('repdate', names(fac))]
  repp <- as.data.frame(lapply(rep, ymd))
  
    #Subset antibiotic prescription date and convert it to readable date with ymd
  ant <- fac[, grep('anti\\dstart', names(fac))]
  antt <- as.data.frame(lapply(ant, ymd))
  
    #Create a container to store binary result describing whether condition was met
  result <- matrix(data = 0, nrow = nrow(rep), ncol = (ncol(rep)))
  
    #Insert '1' in results when repp column is greater then 3 antt columns. Note that
    #the double brackets [[]] around repp allow it to be compared as a single element
    #to the antt results, which have three corresponding columns with a reportable
    #antibiotic. 
  for (j in 1:nrow(repp)) {
    for (i in 1:ncol(repp)) {
      if ( any(repp[[j,i]] > antt[j,(((i*3)-2):(i*3))], na.rm = T) ) {
        result[j,i] <- 1
      } 
    }
  }
  rowSums(result)
}


# ANTIBIOTICS PRESCRIPTIONS ISSUED (For presumed UTI)
# Row sums are printed for all columns with 'anticnt'. These columns represent the number of 
# prescriptions given for a patient. Similar to the Res_Days function (above), all rows are summed with 
# rowSums(), grep() subsets out the 'anticnt' name, and NAs are removed. 

abx_pre <- function(fac) {
  rowSums(fac[, grep('anticnt', names(fac))], na.rm = T)
}

# PATIENTS PRESDCRIBED AN ANTIBIOTIC (For presumed UTI)
# Row sums are printed for all columns with 'anticnt'.

ptn_pre <- function(fac) {
  a <- fac[, grep('anticnt', names(fac))]
  a[a == 2]<-1
  a[a == 3]<-1
  a[a == 4]<-1
  rowSums(a, na.rm = T)
}

# ANTIBIOTIC DAYS OF THERAPY (For presumed UTI)
# Row sums are printed for all columns with 'anti#days', representing the days of therapy (DOT) for 
# each antibiotic prescribed. The # sign ranges from 1-3 as three DOT counts can be reported per patient. 
# This function therefore sums the total number of days, for all patients, for all antibiotics prescribed 
# into a single count. Similar to the Res_Days function (above), rows are summed with rowSums(), grep() 
# subsets out the 'anti\\ddays' name, and NAs are removed. The \\d is included between org and cnt to 
# ignore numbers 1-3, and include all columns with these names. 

abx_dot <- function(fac) {
  rowSums(fac[, grep('anti\\ddays', names(fac))], na.rm = T)
}

# POSITIVE ISOLATES FOR C. DIFF INFECTION
# A single question on the survey asks for the total number of positive clostridium difficile infections 
# for the month. Because this is a single question, the grep() function is not necessary. Instead, rowSums() 
# is applied to transform the tibble into a vector and make NA's 0. 

cdi <- function(fac) {
  rowSums(fac[,"cdiff_cultures"], na.rm = T)
}

# URINE CULTURE & SENSITIVITY TESTS ORDERED MEETING CLINICAL SYMPTOMS
# The purpose of this function is to count the number urine culture tests ordered that met clinical symptoms. 
# There are two types of patients to consider, those with and without indwelling catheters. The first section 
# of the function determines whether patients without an indwelling catheter met clinical criteria. The second 
# section of the function determines whether patients with a catheter met clinical criteria.

# Clinical criteria is met when a constellation of symptoms are observed and reported by a physician. See the
# UTI_Criteria.pdf attachment section A Part 1 (For residents without indwelling catheters) and section B Part 1 
# (For residents with an indwelling catheter) for criteria details. 

# The dataset 'nocath' subsets all reported symptom columns for patients without a catheter. There are 13 
# columns reportable for each patient, with nocathsym___1 - 9 included in the formal surveillance criteria. The 
# variable name for each symptoms is listed below: 

# nocathsym___1 == Acute dysuria 
# nocathsym___2 == Acute pain, swelling, or tenderness of the testes, epididymis, or prostate
# nocathsym___3 == Fever or leukocytosis
# nocathsym___4 == Acute costovertebral angle pain or tenderness
# nocathsym___5 == Suprapubic pain
# nocathsym___6 == Gross hematuria
# nocathsym___7 == New or marked increase in incontinence
# nocathsym___8 == New or marked increase in urgency
# nocathsym___9 == New or marked increase in frequency
# nocathsym___10 == Foul urine (Note: this is not part of the Mcgeer criteria so is not considered below)
# nocathsym___11 == Dark urine (Note: this is not part of the Mcgeer criteria so is not considered below)
# nocathsym___12 == Other (Note: this is not part of the Mcgeer criteria so is not considered below)
# nocathsym___13 == None (Note: this is not part of the Mcgeer criteria so is not considered below)

# The 'result' container is then created. A for loop adds 1 when a sufficient constellation of symptoms is met. 
# There are three ways the symptoms can be met. 1) nocathsym___1 OR nocathsym___2 == 1; 2) nocathsym___3 AND at 
# least one of nocathsym___4 OR nocathsym___5 OR nocathsym___6 OR nocathsym___7 OR nocathsym___8 OR nocathsym___9; 
# 3) At least two of nocathsym___5 OR nocathsym___6 OR nocathsym___7 OR nocathsym___8 OR nocathsym___9. These 
# sufficient combinations are described in detail in the UTI_Criteria.pdf attachment. 

urin_clin <- function(fac) {
  
  nocath <- fac[,grep('nocathsym_',names(fac))]
  result <- matrix(data = 0, nrow = nrow(nocath), ncol = (ncol(nocath)/15))
  
  for (j in 1:nrow(nocath)) {
    for (i in (1:((ncol(nocath))/15))) {
      if (
        ((nocath[j,((i*15)-14)] == 1) | ((nocath[j,((i*15)-13)]) == 1)) | 
        ((nocath[j,((i*15)-12)] == 1) & (((nocath[j,((i*15)-11)]) + 
                                          (nocath[j,((i*15)-10)]) + 
                                          (nocath[j,((i*15)-9)]) +
                                          (nocath[j,((i*15)-8)]) +
                                          (nocath[j,((i*15)-7)]) +
                                          (nocath[j,((i*15)-6)])) >= 1)) |
        ( ((nocath[j,((i*15)-10)]) +
           (nocath[j,((i*15)-9)]) +
           (nocath[j,((i*15)-8)]) +
           (nocath[j,((i*15)-7)]) +
           (nocath[j,((i*15)-6)])) >= 2 )
      ) {
        result[j,i] <- 1
      } 
    }
  }

  # For patients with a catheter the process is repeated, with a different constellation supporting clinical
  # criteria. First, the 'cath' dataset subsets out all symptom variables for cultured patients with an 
  # indwelling catheter. The 'result2' container is established matching the number of rows of 'cath', and 
  # creating a single column for every 8 possible symptoms. The for loop assess whether at least one of 
  # the reported symptoms was reported (shown as 1 in 'cath'), and prints 1 to the 'result2' column. There 
  # are 8 columns for each patient, each representing the following symptoms (only the first 4 are included 
  # in the assessment):
  
  # cathsym___1 == Fever, rigors, or new-onset hypotension, with no alternate site of infection
  # cathsym___2 == Either acute change in mental status or acute functional decline, with no alternate 
  #   diagnosis and leukocytosis
  # cathsym___3 == New-onset suprapubic pain or costovertebral angle pain or tenderness
  # cathsym___4 == Purulent discharge from around the catheter or acute pain, swelling, or tenderness of the 
  #   testes, epididymis, or prostate
  # cathsym___5 == Foul urine (Note: this is not part of the Mcgeer criteria so is not considered below)
  # cathsym___6 == Dark urine (Note: this is not part of the Mcgeer criteria so is not considered below)
  # cathsym___7 == Other (Note: this is not part of the Mcgeer criteria so is not considered below)
  # cathsym___8 == None (Note: this is not part of the Mcgeer criteria so is not considered below)
  
  cath <- fac[,grep('^cathsym_',names(fac))]
  result2 <- matrix(data = 0, nrow = nrow(cath), ncol = (ncol(cath)/11))
  
  for (x in 1:nrow(cath)) {
    for (y in (1:((ncol(cath))/11))) {
      if( 
        ((cath[x,((y*11)-10)]) == 1) |
        ((cath[x,((y*11)-9)]) == 1) |
        ((cath[x,((y*11)-8)]) == 1) |
        ((cath[x,((y*11)-7)]) == 1)
      ) {
        result2[x,y] <- 1 
      }
    }
  }
  #Bind catheter and non-cather datasets by column and sum the rows
  rowSums(cbind(result,result2))
}

# URINE CULTURE & SENSITIVITY TESTS ORDERED MEETING MICROBIOLOGIC CRITERIA
# The purpose of this function is to count the number of patients that met the McGeer criteria based on 
# microbiologic subcriteria. Subcriteria are listed in the UTI_Criteria.pdf attachment section A Part 2A 
# (For residents without indwelling catheters) and section B Part 2 (For residents with an indwelling 
# catheter). These criteria differ for patients without a catheter and patients with a catheter. As in the 
# functions above, "org\\dcnt" variables represent various lab culture results. Each four columns in 'mic' 
# (below) represents data from the same patient and contains a numeric from 0-3 representing various urine 
# culture bacterial counts. The CFU response to the numeric values in 'mic' are listed below:

# 0 == no lab coulter
# 1 == <50,000 CFUs
# 2 == 50,000 - 100,000 CFUs
# 3== >100,000 CFUs.

# Because microbiologic data for both catheter and non-catheter patients, and subcriteria are different 
# for the two patient types, a separate variable is used to determine catheter status. In the code below 
# the new dataframe 'catheter' is created to assess catheter status. The dataframe 'mic' is then created 
# to subset out urine culture results. The 'result' container is created. The for loop then assess if at 
# least 100,000 CFUs of no more than 2 species of microorganisms in a voided urine sample. In addition, 
#the 'catheter' dataframe must equal '0', indicating that the patient did not have a catheter. Within 
# the loop, only a minimum combination of the numeric '1' and '2' is sufficient for a patient to meet 
# the condition. This is because three '1' values may fall well below 50,000 CFUs each, thereby not 
# reaching the 100,000 CFU minimum threshold. 

micro <- function(fac) {
  #Catheter status: variable determining if patient had no catheter (0) or a catheter (1)
  catheter <- fac[ ,grep('cathind',names(fac))] 
  catheter[is.na(catheter)] <- 0
  
  #Subset out all urine culture results
  mic <- fac[ ,grep('org\\dcnt',names(fac))] 
  mic[is.na(mic)] <- 0
  
  #Create the first container for non-catheter patients
  result <- matrix(data = 0, nrow = nrow(mic), ncol = (ncol(mic)/4))
  
  #Loop to assess if microbiologic criteria was met for non-catheter patients
  for (q in 1:nrow(mic)) {
    for (r in (1:((ncol(mic))/4))) {
      if( 
        (((mic[q,((r*4)-3)]) + (mic[q,((r*4)-2)]) > 2) |
         ((mic[q,((r*4)-3)]) + (mic[q,((r*4)-1)]) > 2) |
         ((mic[q,((r*4)-3)]) + (mic[q,(r*4)]) > 2) |
         ((mic[q,((r*4)-2)]) + (mic[q,((r*4)-1)]) > 2) |
         ((mic[q,((r*4)-2)]) + (mic[q,(r*4)]) > 2) |
         ((mic[q,((r*4)-1)]) + (mic[q,(r*4)]) > 2)) &
        (catheter[q,r] == 0)
      ) {
        result[q,r] <- 1 
      }
    }
  }
  
  # This process is repeated for patients with a catheter, however, the sufficient criteria to meet the 
  # microbiologic subcriteria is reduced. The urine culture must report a culture with at least 100,000 
  # CFUs from any organism(s). As before, the 'result2' container is created. The for loop then assess 
  # if the sum of all four patient columns is greater than two, AND if the patient had a catheter (1). 
  
  #Create a second container for catheter patients
  result2 <- matrix(data = 0, nrow = nrow(mic), ncol = (ncol(mic)/4))
  
  #Loop to assess if microbiologic criteria was met for catheter patients
  for (h in 1:nrow(mic)) {
    for (j in (1:((ncol(mic))/4))) {
      if( (sum(mic[h, (((j*4)-3):(j*4)) ]) > 2) & (catheter[h,j] == 1)) {
        result2[h,j] <- 1 
      }
    }
  }
  # The 'result' and 'result2' dataframe columns are combined and summed by row. 
  rowSums(cbind(result,result2))
}

# NUMBER OF PATIENTS MEETING THE MCGEER CRITERIA
# This is function is a combination of the 'urine_clin' and 'micro' functions above and is a programmed 
# to apply the UTI_Criteria.pdf to the data collected in this study. These criteria are referred to as 
# the mcgeer criteria. As in the functions before, non-catheter and catheter patients are assessed 
# separately in distinct for loops. 

# First, clinical symptoms are subset into the 'nocath' dataframe. Microbiology symptoms are then subset 
# into the 'mic' dataframe. The 'catheter' dataframe is additionally included, as assessing microbiologic 
# results includes catheter status (see 'micro' function notes above). The container 'result' is then 
# established to store loop data. The for loop evaluates if both clinical and microbiologic criteria are 
# met. Although the two dataframes have a different number of columns, they are looped over the same 
# number of times. If both conditions are met '1' is printed to the 'result' container. 

mcgeer <- function(fac) {
  
  #Clinical symptoms noted for non-catheter patient
  nocath <- fac[,grep('nocathsym_',names(fac))]
  
  #Microbiology
  #Catheter status: variable determining if patient had no catheter (0) or a catheter (1)
  catheter <- fac[ ,grep('cathind',names(fac))] 
  catheter[is.na(catheter)] <- 0
  
  #Urine culture results
  mic <- data.frame(fac[ ,grep('org\\dcnt',names(fac))]) 
  mic[is.na(mic)] <- 0
  
  #Container
  #Create the first container for non-catheter patients
  result <- matrix(data = 0, nrow = nrow(mic), ncol = (ncol(mic)/4))
  
  #Loop to assess if clinical AND microbiologic criteria was met for NON-CATHETER patients
  for (j in 1:nrow(nocath)) {
    for (i in (1:((ncol(nocath))/15))) {
      if ((
        ((nocath[j,((i*15)-14)] == 1) | ((nocath[j,((i*15)-13)]) == 1)) | 
        ((nocath[j,((i*15)-12)] == 1) & (((nocath[j,((i*15)-11)]) + 
                                          (nocath[j,((i*15)-10)]) + 
                                          (nocath[j,((i*15)-9)]) +
                                          (nocath[j,((i*15)-8)]) +
                                          (nocath[j,((i*15)-7)]) +
                                          (nocath[j,((i*15)-6)])) >= 1)) |
        ( ((nocath[j,((i*15)-10)]) +
           (nocath[j,((i*15)-9)]) +
           (nocath[j,((i*15)-8)]) +
           (nocath[j,((i*15)-7)]) +
           (nocath[j,((i*15)-6)])) >= 2 )
      ) & ( 
        (((mic[j,((i*4)-3)]) + (mic[j,((i*4)-2)]) > 2) |
         ((mic[j,((i*4)-3)]) + (mic[j,((i*4)-1)]) > 2) |
         ((mic[j,((i*4)-3)]) + (mic[j,(i*4)]) > 2) |
         ((mic[j,((i*4)-2)]) + (mic[j,((i*4)-1)]) > 2) |
         ((mic[j,((i*4)-2)]) + (mic[j,(i*4)]) > 2) |
         ((mic[j,((i*4)-1)]) + (mic[j,(i*4)]) > 2)) &
        (catheter[j,i] == 0)
      )) {
        result[j,i] <- 1
      } 
    }
  }
  
# Assessing catheter patients
# The process is repeated for catheter patients. A new dataframe is established, 'cath' as these symptoms
# capture different variables than 'nocath'. The 'result2' container is then created to store loop 
# results. Finally, a loop including both clinical and microbiologic criteria is run on the 'cath' and 
# 'mic' datasets. If both conditions are met, '1' is printed to the 'result2' dataset. Both resulting 
# dataframes are combined and summed by row. 
  
  #Clinical symptoms noted for catheter patient
  cath <- fac[,grep('^cathsym_',names(fac))]
  
  #Container
  #Create the second container for catheter patients
  result2 <- matrix(data = 0, nrow = nrow(cath), ncol = (ncol(cath)/11))
  
  #Loop to assess if clinical AND microbiologic criteria was met for CATHETER patients
  for (x in 1:nrow(cath)) {
    for (y in (1:((ncol(cath))/11))) {
      if(( 
        ((cath[x,((y*11)-10)]) == 1) |
        ((cath[x,((y*11)-9)]) == 1) |
        ((cath[x,((y*11)-8)]) == 1) |
        ((cath[x,((y*11)-7)]) == 1)
      ) & ((sum(mic[x, (((y*4)-3):(y*4)) ]) > 2) & (catheter[x,y] == 1))) {
        result2[x,y] <- 1 
      }
    }
  }
  
  rowSums(cbind(result,result2))
}

#NUMBER OF ANTIBIOTICS PRESCRIBED WITHOUT MEETING THE MCGEER CRITERIA 
ant_pre_nomcgeer <- function(fac) {
  #Calcuate the number of patients meeting the McGeer criteria
  #1) Non-catheter patients meeting the McGeer criteria 
  nocath <- data.frame(fac[,grep('nocathsym_',names(fac))])
  
  catheter <- data.frame(fac[ ,grep('cathind',names(fac))])
  catheter[is.na(catheter)] <- 0
  
  mic <- data.frame(fac[ ,grep('org\\dcnt',names(fac))]) 
  mic[is.na(mic)] <- 0
  
  result <- matrix(data = 0, nrow = nrow(mic), ncol = (ncol(mic)/4))
  
  for (j in 1:nrow(nocath)) {
    for (i in (1:((ncol(nocath))/15))) {
      if ((
        ((nocath[j,((i*15)-14)] == 1) | ((nocath[j,((i*15)-13)]) == 1)) | 
        ((nocath[j,((i*15)-12)] == 1) & (((nocath[j,((i*15)-11)]) + 
                                          (nocath[j,((i*15)-10)]) + 
                                          (nocath[j,((i*15)-9)]) +
                                          (nocath[j,((i*15)-8)]) +
                                          (nocath[j,((i*15)-7)]) +
                                          (nocath[j,((i*15)-6)])) >= 1)) |
        ( ((nocath[j,((i*15)-10)]) +
           (nocath[j,((i*15)-9)]) +
           (nocath[j,((i*15)-8)]) +
           (nocath[j,((i*15)-7)]) +
           (nocath[j,((i*15)-6)])) >= 2 )
      ) & ( 
        (((mic[j,((i*4)-3)]) + (mic[j,((i*4)-2)]) > 2) |
         ((mic[j,((i*4)-3)]) + (mic[j,((i*4)-1)]) > 2) |
         ((mic[j,((i*4)-3)]) + (mic[j,(i*4)]) > 2) |
         ((mic[j,((i*4)-2)]) + (mic[j,((i*4)-1)]) > 2) |
         ((mic[j,((i*4)-2)]) + (mic[j,(i*4)]) > 2) |
         ((mic[j,((i*4)-1)]) + (mic[j,(i*4)]) > 2)) &
        (catheter[j,i] == 0)
      )) {
        result[j,i] <- 1
      } 
    }
  }
  
  #2) Catheter patients meeting the McGeer criteria 
  cath <- fac[,grep('^cathsym_',names(fac))]
  
  result2 <- matrix(data = 0, nrow = nrow(cath), ncol = (ncol(cath)/11))
  
  for (x in 1:nrow(cath)) {
    for (y in (1:((ncol(cath))/11))) {
      if(( 
        ((cath[x,((y*11)-10)]) == 1) |
        ((cath[x,((y*11)-9)]) == 1) |
        ((cath[x,((y*11)-8)]) == 1) |
        ((cath[x,((y*11)-7)]) == 1)
      ) & ((sum(mic[x, (((y*4)-3):(y*4)) ]) > 2) & (catheter[x,y] == 1))) {
        result2[x,y] <- 1 
      }
    }
  }
  
  #Sum the McGeer criteria patients into a single dataframe
  result3 <- result + result2
  
  #3) Determine if an antibiotic was prescribed 
  result_presc <- fac[, grep('anticnt', names(fac))]
  result_presc[is.na(result_presc)] <- 0
  
  #Create a final container
  result4 <- matrix(data = 0, nrow = nrow(result_presc), ncol = ncol(result_presc))
  
  #Create a final for loop counting whether an antibiotic was prescribed in 'result_presc',
  #but failed to meet the McGeer criteria in 'result3'
  for (x in 1:nrow(result_presc)) {
    for (y in 1:ncol(result_presc)) {
      if( (result3[x,y] == 0) & (result_presc[x,y] == 1) ) {
        result4[x,y] <- 1
      } else if ( (result3[x,y] == 0) & (result_presc[x,y] == 2) ) {
        result4[x,y] <- 2
      }
    }
  }
  rowSums(result4)
}



# COST OF ANTIBIOITCS 

cost <- function(fac) {
  
  name_list <- data.frame(fac[grep("^(?=.*anti\\dname)(?!.*unlist)", names(fac), perl=TRUE)])
  name_list[is.na(name_list)] <- "NA"
  
  name_unls <- data.frame(fac[grep("^(?=.*anti\\dnameunlist)", names(fac), perl=TRUE)])
  name_unls[is.na(name_unls)] <- "NA"
  
  dose <- data.frame(lapply(fac[, grep('anti\\ddose', names(fac))], as.numeric))
  dose[is.na(dose)] <- 0

  result <- as.data.frame(matrix(data = 0, nrow = nrow(name_list), ncol = ncol(name_list)))
  
  for (q in 1:nrow(name_list)) {
    for (r in 1:ncol(name_list)) {
      if ( (name_list[q,r]  == "Cefalexin") | (name_unls[q,r]  == "Cefalexin")) {
        result[q,r] <- 0.04248*dose[q,r]
      } else if ( (name_list[q,r]  == "Augmentin") | (name_unls[q,r]  == "Augmentin")) {
        result[q,r] <- 2.59204*dose[q,r]
      } else if ( (name_list[q,r]  == "Amoxicillin") | (name_unls[q,r]  == "Amoxicillin")) {
        result[q,r] <- 0.03976*dose[q,r]
      } else if ( (name_list[q,r]  == "Bactrim") | (name_unls[q,r]  == "Bactrim")) {
        result[q,r] <- 1.999875*dose[q,r]
      } else if ( (name_list[q,r]  == "Cipro") | (name_unls[q,r]  == "Cipro")) {
        result[q,r] <- 0.58256*dose[q,r]
      } else if ( (name_list[q,r]  == "Ciprofloxacin") | (name_unls[q,r]  == "Ciprofloxacin")) {
        result[q,r] <- 1.154*dose[q,r]
      } else if ( (name_list[q,r]  == "Keflex") | (name_unls[q,r]  == "Keflex")) {
        result[q,r] <- 1.834*dose[q,r]
      } else if ( (name_list[q,r]  == "Levaquin") | (name_unls[q,r]  == "Levaquin")) {
        result[q,r] <- 3.15468*dose[q,r]
      } else if ( (name_list[q,r]  == "Macrobid") | (name_unls[q,r]  == "Macrobid")) {
        result[q,r] <- 6.3051*dose[q,r]
      } else if ( (name_list[q,r]  == "Nitrofurantoin(Bs)") | (name_unls[q,r]  == "Nitrofurantoin(Bs)")) {
        result[q,r] <- 16.9084*dose[q,r]
      } 
    }
  }
  rowSums(result)
}

# RETURN A DATAFRAME APPLYING FUNCTIONS
# A final function is used to combine all the functions above into a final dataframe. The dataframe is 
# subsequently used to support some of the in-line text and graphics in the associated markdown document.


report <- function(fac){
  data.frame(fac$rep_date,
             Res_Days(fac),
             order_cnt(fac),
             abx_dot(fac),
             Month_Int(fac),
             Urine_CS(fac),
             urine_pos(fac),
             urine_poslow(fac),
             urine_posany(fac),
             uti_emp(fac), # ANTIBIOTICS PRESCRIBED PRIOR TO LAB RESULT AVAILABILITY 
             abx_pre(fac), # ANTIBIOTICS PRESCRIPTIONS ISSUED (For presumed UTI)
             ptn_pre(fac),
             cdi(fac),
             cost(fac), # COST OF ANTIBIOITCS 
             urin_clin(fac), # URINE CULTURE & SENSITIVITY TESTS ORDERED MEETING CLINICAL SYMPTOMS
             ant_pre_nomcgeer(fac), #NUMBER OF ANTIBIOTICS PRESCRIBED WITHOUT MEETING THE MCGEER CRITERIA 
             mcgeer(fac))  # NUMBER OF PATIENTS MEETING THE MCGEER CRITERIA
}

###############################
# GRAPHING FUNCTIONS (Part 2) #
###############################

#COST OF ANTIBIOTICS BY ANTIBIOTIC TYPE AND FREQUENCY PRESCRIBED 
cost_table <- function(fac) {
  
  name_list <- data.frame(fac[grep("^(?=.*anti\\dname)(?!.*unlist)", names(fac), perl=TRUE)])
  name_list[is.na(name_list)] <- "NA"
  
  dose <- data.frame(fac[, grep('anti\\ddose', names(fac))]) 
  dose[is.na(dose)] <- 0
  
  result <- as.data.frame(matrix(data = 0, nrow = nrow(name_list), ncol = ncol(name_list)))
  
  for (q in 1:nrow(name_list)) {
    for (r in 1:ncol(name_list)) {
      if ( (name_list[q,r]  == "Cefalexin") ) {
        result[q,r] <- 0.04248*dose[q,r]
      } else if ( (name_list[q,r]  == "Augmentin") ) {
        result[q,r] <- 2.59204*dose[q,r]
      } else if ( (name_list[q,r]  == "Amoxicillin") ) {
        result[q,r] <- 0.03976*dose[q,r]
      } else if ( (name_list[q,r]  == "Bactrim") ) {
        result[q,r] <- 1.999875*dose[q,r]
      } else if ( (name_list[q,r]  == "Cipro") ) {
        result[q,r] <- 0.58256*dose[q,r]
      } else if ( (name_list[q,r]  == "Ciprofloxacin") ) {
        result[q,r] <- 1.154*dose[q,r]
      } else if ( (name_list[q,r]  == "Keflex") ) {
        result[q,r] <- 1.834*dose[q,r]
      } else if ( (name_list[q,r]  == "Levaquin") ) {
        result[q,r] <- 3.15468*dose[q,r]
      } else if ( (name_list[q,r]  == "Macrobid") ) {
        result[q,r] <- 6.3051*dose[q,r]
      } else if ( (name_list[q,r]  == "Nitrofurantoin(Bs)") ) {
        result[q,r] <- 16.9084*dose[q,r]
      } 
    }
  }
  
  list_name <- as.character(as.vector(t(name_list)))
  list_cost <- as.vector(t(result))
  
  list_bind <- data.frame(list_name,list_cost)
  
  list_table <- list_bind %>%
    group_by(list_name) %>% 
    mutate(., count = ifelse(list_name=="NA",0,1)) %>%
    summarise_all(funs(sum)) %>%
    select(list_name, count, list_cost) %>%
    filter(., list_name != "NA")
  
  list_table2 <- list_table %>%
    rename("Antibiotic Name" = list_name, "Total Prescribed" = count, "Wholesale Cost" = list_cost)
  
  as.data.frame(list_table2)
}

pietable <- function(piet) {
  #Create a pie graph of the appropriate vs. innapropriate presctiptions
  piedata <- data.frame(Category = c("Met \n Criteria \n","Failed to \n Meet Criteria \n"),
                        "freq" = c( piet$mcgeer.fac.[1],
                                   piet$ptn_pre.fac.[1] - piet$mcgeer.fac.[1]))
  
  p <- ggplot(piedata, aes (x="", y = freq, fill = factor(Category))) + 
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(freq / sum(freq) * 100),"% ", Category), x = 1.0),
              position = position_stack(vjust = 0.5), size = 5, color = "white") +
    theme_classic() +
    scale_fill_manual(values=c("thistle4","seagreen3")) +
    theme(plot.title = element_text(vjust=-7, hjust=0.5, size = 17),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none") +
    labs(fill = "Category",
         x = NULL,
         y = NULL #,title = ""
    ) + 
    coord_polar("y")
  
  ggsave("pie_chart.png", p, bg = "transparent", units = "in" , width = 4.5, height = 4.5,  path = "C:/Users/Person/OneDrive/z1_Research/ABRC/images")
}

##########################################
# QUARTERLY REPORT IN-LINE TEXT (Part 3) #
##########################################

res.days <- function(res.days) {
  paste0(res.days$Res_Days.fac.[1], " total resident days")
}

ant.days <- function(ant.days) {
  paste0(ant.days$abx_dot.fac.[1])
}

test.ord <- function(test.ord) {
  paste0(test.ord$Urine_CS.fac.[1])
}

high.test <- function(high.test) {
  paste0(high.test$urine_pos.fac.[1])
}

med.test <- function(med.test) {
  paste0(med.test$urine_poslow.fac.[1])
}

ant.presc <- function(ant.presc) {
  paste0(ant.presc$abx_pre.fac.[1])
}

ptn.presc <- function(ptn.presc) {
  paste0(ptn.presc$ptn_pre.fac.[1])
}

clin_scr <- function(clin_scr) {
  paste0(clin_scr$urin_clin.fac.[1])
}

ant_scr <- function(ant_scr) {
  paste0(ant_scr$ant_pre_nomcgeer.fac.[1])
}


numb.prec2 <- function(numb.prec2) {
  dat1 <- cost_table(numb.prec2[1:2,])
  sum(dat1[,2])
}

  
  
  
  
