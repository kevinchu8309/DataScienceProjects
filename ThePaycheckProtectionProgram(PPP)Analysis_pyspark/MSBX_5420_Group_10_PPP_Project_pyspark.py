%%configure -f
{"executorMemory": "4G","driverMemory":"8G","executorCores": 1,"numExecutors": 3, 
 "conf": {"spark.dynamicAllocation.enabled": "false", 
          "spark.sql.parquet.enableVectorizedReader": "false", 
          "spark.pyspark.python": "python3",
          "spark.pyspark.virtualenv.enabled": "true",
          "spark.pyspark.virtualenv.type": "native",
          "spark.pyspark.virtualenv.bin.path": "/usr/bin/virtualenv"}}

from pyspark.sql import SparkSession
spark = SparkSession.builder.master('local[*]').config("spark.driver.memory", "2g").appName('spark_ppp_data').getOrCreate()

spark_df = spark.read.options(delimiter=',', header=True, inferSchema=True, multiLine=True, escape='\"')\
        .csv("s3://msbx5420-spr23/team_10/public_150k_plus_230101.csv")

type(spark_df)

sc.install_pypi_package("pandas")
sc.install_pypi_package("missingno")
sc.install_pypi_package("matplotlib")
sc.install_pypi_package("seaborn")
sc.install_pypi_package("plotly")
sc.install_pypi_package("pylab")
sc.install_pypi_package("numpy")


# Loading all the packages needed for the analysis
import pandas as pd                           #pandas for data wrangling/preparation
import missingno as msno                      #Python library for the exploratory visualization of missing data
import matplotlib.pyplot as plt               #library for matplotlib visualization
import matplotlib.dates as mdates             #for time series plot
import seaborn as sns                         #library for seaborn visualization
import plotly.express as px                   #library for plotly visualization
from pylab import rcParams 
import numpy as np

rcParams['figure.figsize'] = 10,8             #setting default size of the plot
sns.set(font_scale = 1.2, style = 'ticks')

## Only use on google colab
# from google.colab import drive
# drive.mount('/content/drive')
# ppp_df = pd.read_csv('/content/drive/MyDrive/CU BOULDER/public_150k_plus_230101.csv')

# ppp_df = pd.read_csv('/content/public_150k_plus_230101.csv')
# Convert pyspark df to pandas df for wrangling first.
ppp_df = spark_df.toPandas()

len(ppp_df)

ppp_df.head()

ppp_df.tail()

# Overview of the Dataframe
ppp_df.info()

# Now, Dropping the columns from this dataframes which are biased, having many null/NaN/NA values and data that are not relevant for this analysis.
# Using the drop function using the index provided above in info()
ppp_df.drop(ppp_df.columns[[2, 5, 6, 8, 9, 12, 15, 16, 17, 19, 20, 21, 22, 25, 27, 28, 29, 30, 31, 33, 34, 
                            35, 36, 38, 39, 40, 41, 42, 44, 46, 47, 48, 49, 50]], axis = 1, inplace = True)

# Viewing to confirm whether the changes are done or not.
ppp_df.info()

# Head of the dataframe after droping columns.
ppp_df.head()

# Changing the datatype of the date columns from object/string(MM/DD/YYYY) to datetime64(YYYY-MM-DD)
ppp_df[['DateApproved', 'ForgivenessDate']] = ppp_df[['DateApproved', 'ForgivenessDate']].apply(pd.to_datetime)
ppp_df.info()
ppp_df.head()

# Checking how many columns have NA/NaN/Null values and caluculating total NA values for all columns
ppp_df.isna().sum()

# Using the matrix method of missing no to view all NA values.
msno.matrix(ppp_df)
plt.show()
%matplot plt

# Dropping all NA values in the dataframe
ppp_df.dropna(inplace = True)

# Resetting the index after droping NA values
ppp_df.reset_index(inplace = True, drop = True)
ppp_df.info()

# Checking if there are any duplicate values present in the dataframe
ppp_df.duplicated().sum()

# Checking for unique values in columns which are limited to certain data only.
column_list = [2, 5, 10, 11, 12, 15]

for i in column_list:
    print(f'\033[1m{ppp_df.columns[i]} : \033[0m\n {ppp_df[ppp_df.columns[i]].unique()}')
# \033[1m, \033[0m - to start, end bold

ppp_df.describe()

# Exporting the cleaned dataframe in a csv file
# ppp_df.to_csv('/content/drive/MyDrive/CU BOULDER/public_150k_plus_cleaned.csv')
# ppp_df.to_csv('./public_150k_plus_cleaned.csv')

# Checking the loan amount min and max values
print(ppp_df['CurrentApprovalAmount'].min(), ppp_df['CurrentApprovalAmount'].max())

# min is 150K and max is 10M, so creating a category for a range of loan amount.
ppp_df['LoanRange'] = pd.cut(ppp_df['CurrentApprovalAmount'], 
                             bins = [0, 350000, 1000000, 2000000, 5000000, float('inf')], 
                             labels = ['Less than 350K', '350K - 1M', '1M - 2M', '2M - 5M', 'More than 5M'])
ppp_df['LoanRange'].unique()

# Visualizing the loan category data
sns.catplot(data = ppp_df, y = 'LoanRange', kind = 'count', palette = 'Blues', height = 7, aspect = 1.5)
plt.title('Loan Amount Category Wise Count')
plt.ylabel('Loan Range')
plt.show()
%matplot plt


# Reviewing the employees count
plt.clf()
%matplot plt
sns.histplot(data = ppp_df, x = 'JobsReported', bins = 50, color = 'LightBlue')
plt.title('Overall Employee Count')
plt.xlabel('Jobs Reported')
plt.show()
%matplot plt

# Viewing both the data together Loan Category vs Employees Count, grouping on Loan Category
ppp_gb_df = ppp_df.groupby('LoanRange').sum()
ppp_gb_df.reset_index(inplace = True)
ppp_gb_df.head()

# Vizualizing Loan Category with Employees Count
sns.barplot(data = ppp_gb_df, x = 'LoanRange', y = 'JobsReported', color = 'darkblue')
plt.title('Loan Category with Employee Count')
plt.ylabel('Jobs Reported')
plt.xlabel('Loan Range')
plt.show()
%matplot plt

# Looking at the state details of the borrower.
ppp_state_df = ppp_df.groupby('BorrowerState').sum()
ppp_state_df.reset_index(inplace = True)

# Sorting state wise highest loan borrowers.
ppp_state_df.sort_values('CurrentApprovalAmount', ascending = False)[:5]

# Vizualizing the state data on map plot by plotly.
px.choropleth(ppp_state_df, locations = 'BorrowerState', locationmode = 'USA-states', scope = 'usa', 
              color = 'CurrentApprovalAmount', color_continuous_scale = 'plasma_r',
              title = 'State wise loan amount borrowed', labels = {'BorrowerState': 'State', 'CurrentApprovalAmount': 'Total Loan Amount'})


# Viewing the top 5 states on a bar plot.
plt.clf()
%matplot plt
sns.barplot(data = ppp_state_df, x = 'CurrentApprovalAmount', y = 'BorrowerState', 
            order = ppp_state_df.sort_values('CurrentApprovalAmount', ascending = False)[:5]['BorrowerState'],
           color = 'LightBlue')
plt.title('Top 5 States with Highest Loan Amount')
plt.xlabel('Loan Amount (In 10 Billions)')
plt.show()
%matplot plt

# Checking the top Loan Lenders
plt.clf()
%matplot plt
sns.countplot(data = ppp_df, y = 'OriginatingLender', order = ppp_df['OriginatingLender'].value_counts().index[:15], color = 'Green')
plt.show()
%matplot plt

# Looking at which business type has borrowed Loan
plt.clf()
%matplot plt
sns.countplot(data = ppp_df, y = 'BusinessType', order = ppp_df['BusinessType'].value_counts().index)
plt.show()
%matplot plt

# Filtering
ppp_business_df = ppp_df[ppp_df['BusinessType'].isin(['Corporation', 'Limited  Liability Company(LLC)','Subchapter S Corporation', 'Non-Profit Organization'])]

# Sorting as per the date approved
ppp_business_df = ppp_business_df.sort_values(['DateApproved']).reset_index(drop = True)

# Grouping the data as per approved date and business type on count and cumulative sum aggregate function
ppp_biz_count_df = ppp_business_df.groupby(['DateApproved', 'BusinessType']).count().groupby(level = -1).cumsum().reset_index()

# Grouping the data as per approved date and business type on sum aggregate function
ppp_biz_sum_df = ppp_business_df.groupby(['DateApproved', 'BusinessType']).sum().reset_index()

ppp_biz_count_df

ppp_biz_sum_df

# Business type among the top 4 were reciving loan on which month at what speed of approval
plt.clf()
%matplot plt
fig, ax = plt.subplots(figsize = (16, 9))
sns.lineplot(ax = ax, data = ppp_biz_count_df, x = 'DateApproved', y = 'CurrentApprovalAmount', hue = 'BusinessType')
ax.set(xlabel = 'Month', ylabel = 'Cummulative Loan Approval Count', title = 'Loan Approval Rate As Per Date')
ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
fig.autofmt_xdate()
plt.show()
%matplot plt

plt.clf()
%matplot plt
fig, ax = plt.subplots(figsize = (16, 9))
sns.lineplot(ax = ax, data = ppp_biz_sum_df, x = 'DateApproved', y = 'CurrentApprovalAmount', hue = 'BusinessType')
ax.set(xlabel = 'Month', ylabel = 'Total Loan Amount(in 10 Billions)', title = 'Total Loan Amount Approved During the Program')
ax.xaxis.set_major_locator(mdates.MonthLocator(interval = 1))
fig.autofmt_xdate()
plt.show()
%matplot plt

# Quickly check the data set 
ppp_df

from pyspark.sql import SparkSession
from pyspark.sql.functions import col
from pyspark.ml.feature import VectorAssembler
from pyspark.ml.regression import LinearRegression
from pyspark.ml.evaluation import RegressionEvaluator

# Create Spark data frame
# Create a SparkSession
spark = SparkSession.builder.appName("Pandas to Spark").getOrCreate()

# Convert Pandas DataFrame to Spark DataFrame
spark_df = spark.createDataFrame(ppp_df)

# First, we just use some numeric variables to build a simple model.
loan_df = spark_df.select(col('CurrentApprovalAmount').cast('double'),
                        col('Term').cast('double'),
                        col('JobsReported').cast('double'),
                        col('PAYROLL_PROCEED').cast('double'))

# Split data into training and testing datasets
train_data, test_data = loan_df.randomSplit([0.8, 0.2], seed=42)

# Create feature vector column
assembler = VectorAssembler(inputCols=['Term', 'JobsReported', 'PAYROLL_PROCEED'], outputCol='features')
train_data = assembler.transform(train_data)
test_data = assembler.transform(test_data)

# Fit linear regression model
lr = LinearRegression(featuresCol='features', labelCol='CurrentApprovalAmount', maxIter=10, regParam=0.3, elasticNetParam=0.8)
lr_model = lr.fit(train_data)

# Evaluate model on testing data
evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='mse')
mse = evaluator.evaluate(lr_model.transform(test_data))
print('MSE: ', mse)
# Evaluate model on testing data
evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='r2')
r2 = evaluator.evaluate(lr_model.transform(test_data))
print('R-squared: ', r2)


# The R-squred is too good to be true. Exclude that and run it again.
loan_df = spark_df.select(col('CurrentApprovalAmount').cast('double'),
                        col('Term').cast('double'),
                        col('JobsReported').cast('double')
                        )

# Split data into training and testing datasets
train_data, test_data = loan_df.randomSplit([0.8, 0.2], seed=42)

# Create feature vector column
assembler = VectorAssembler(inputCols=['Term', 'JobsReported'], outputCol='features')
train_data = assembler.transform(train_data)
test_data = assembler.transform(test_data)

# Fit linear regression model
lr = LinearRegression(featuresCol='features', labelCol='CurrentApprovalAmount', maxIter=10, regParam=0.3, elasticNetParam=0.8)
lr_model = lr.fit(train_data)

# Evaluate model on testing data
evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='mse')
mse = evaluator.evaluate(lr_model.transform(test_data))
print('MSE: ', mse)
# Evaluate model on testing data
evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='r2')
r2 = evaluator.evaluate(lr_model.transform(test_data))
print('R-squared: ', r2)


ppp_df.nunique()

# Preparation 
ppp_df_modeling = ppp_df

# Modify Businesstype, keep 4 main categories and make the rest into 'others'
# create a list of values to keep
values_to_keep = ['Limited  Liability Company(LLC)', 'Non-Profit Organization', 'Corporation', 'Subchapter S Corporation']
# replace values not in the list with 'others'
ppp_df_modeling['BusinessType'] = ppp_df_modeling['BusinessType'].apply(lambda x: x if x in values_to_keep else 'others')
ppp_df_modeling.BusinessType.nunique()

# Modify OriginatingLender, keep 2 main categories and make the rest into 'others'
values_to_keep = ['JPMorgan Chase Bank, National Association', 'Bank of America, National Association']
# replace values not in the list with 'others'
ppp_df_modeling['OriginatingLender'] = ppp_df_modeling['OriginatingLender'].apply(lambda x: x if x in values_to_keep else 'others')
ppp_df_modeling.OriginatingLender.nunique()

# convert dtype foe LoanRange
ppp_df_modeling['LoanRange'] = ppp_df_modeling['LoanRange'].astype('object')


# select cols that used for modeling
ppp_df_modeling = ppp_df_modeling.iloc[:, [2,4,5,6,8,10,11,12,13,15,16]]
ppp_df_modeling.info()

ppp_df_modeling.nunique()

# Now, we can consider the categorical data
## Preparation - encoding for the non-numerical cols.
categorical_cols = ppp_df_modeling.select_dtypes(include=['object']).columns.tolist()
ppp_df_modeling = pd.get_dummies(ppp_df_modeling, columns=categorical_cols)

ppp_df_modeling.head()

ppp_df_modeling.info()

# Export those as a new csv file.
# ppp_df_modeling.to_csv('/content/drive/MyDrive/CU BOULDER/public_150k_plus_forModeling.csv')

# Use spark to build a linear regression
spark = SparkSession.builder \
        .appName("PPP Loan Regressor") \
        .getOrCreate()
ppp_spark_df = spark.createDataFrame(ppp_df_modeling)


# Split data
feature_cols = ppp_spark_df.columns
feature_cols.remove("CurrentApprovalAmount")
assembler = VectorAssembler(inputCols=feature_cols, outputCol="features")
data = assembler.transform(ppp_spark_df)
train_data, test_data = data.randomSplit([0.8, 0.2], seed=42)

# Train lm model
lr = LinearRegression(featuresCol="features", labelCol="CurrentApprovalAmount")
lr_model = lr.fit(train_data)

# predict
predictions = lr_model.transform(test_data)

## Evaluate
# Compute the RMSE
rmse_evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='rmse')
rmse = rmse_evaluator.evaluate(predictions)

# Compute the R-squared
r2_evaluator = RegressionEvaluator(labelCol='CurrentApprovalAmount', predictionCol='prediction', metricName='r2')
r2 = r2_evaluator.evaluate(predictions)

print("RMSE: {:.3f}".format(rmse))
print("R-squared: {:.3f}".format(r2))

# Try another regressor
# import necessary libraries
from pyspark.ml.regression import GBTRegressor
from pyspark.ml.evaluation import RegressionEvaluator
from pyspark.ml.feature import VectorAssembler

# create GBT regressor object
gbt = GBTRegressor(featuresCol='features', labelCol='CurrentApprovalAmount', maxIter=10)

# fit the model on the training data
gbt_model = gbt.fit(train_data)

# make predictions on the testing data
predictions = gbt_model.transform(test_data)

# evaluate the model using RMSE and R2 metrics
evaluator = RegressionEvaluator(
    labelCol='CurrentApprovalAmount',
    predictionCol='prediction',
    metricName='rmse')

rmse = evaluator.evaluate(predictions)
print("GBT RMSE:", rmse)

evaluator = RegressionEvaluator(
    labelCol='CurrentApprovalAmount',
    predictionCol='prediction',
    metricName='r2')

r2 = evaluator.evaluate(predictions)
print("GBT R-squared:", r2)


from pyspark.ml.clustering import KMeans
from pyspark.ml.feature import VectorAssembler, StandardScaler

# Assemble all the features into a vector
assembler = VectorAssembler(inputCols=[col for col in ppp_df_modeling.columns if col != 'CurrentApprovalAmount'],
                            outputCol='features')
ppp_df_modeling = assembler.transform(ppp_spark_df)

# Normalize the data
scaler = StandardScaler(inputCol='features', outputCol='scaled_features')
scaler_model = scaler.fit(ppp_df_modeling)
ppp_df_modeling = scaler_model.transform(ppp_df_modeling)

# Train KMeans clustering model
kmeans = KMeans(featuresCol='scaled_features', k=5, seed=1)
kmeans_model = kmeans.fit(ppp_df_modeling)

# Predict the cluster for each data point
predicted_clusters = kmeans_model.transform(ppp_df_modeling)


from pyspark.ml.evaluation import ClusteringEvaluator

# Assemble all the features into a vector

assembled_data = assembler.transform(ppp_spark_df)

silhouette_scores=[]
evaluator = ClusteringEvaluator(featuresCol='scaled_features', \
metricName='silhouette', distanceMeasure='squaredEuclidean')

for K in range(2,11):
  kmeans = KMeans(featuresCol='scaled_features', k=K, seed=1)
  kmeans_model = kmeans.fit(ppp_df_modeling)
  # predicted_clusters = kmeans_model.transform(ppp_df_modeling)
  # evaluation_score=evaluator.evaluate(predicted_clusters)
  silhouette_scores.append(kmeans_model.summary.trainingCost)
  # silhouette_scores.append(evaluation_score)

plt.clf()
%matplot plt
fig, ax = plt.subplots(1,1, figsize =(10,8))
ax.plot(range(2,11),silhouette_scores)
ax.set_xlabel('Number of Clusters')
ax.set_ylabel('score')
plt.show()
%matplot plt

from pyspark.ml.feature import PCA
from pyspark.ml.clustering import KMeans


# Define PCA object
pca = PCA(k=2, inputCol="scaled_features", outputCol="pca_features")

# Fit PCA model on data
pca_model = pca.fit(ppp_df_modeling)

# Transform data using fitted PCA model
pca_df = pca_model.transform(ppp_df_modeling)

# Define KMeans object
kmeans = KMeans(featuresCol='pca_features', k=7, seed=1)

# Fit KMeans model on transformed data
kmeans_model = kmeans.fit(pca_df)

# Predict the cluster for each data point
predicted_clusters = kmeans_model.transform(pca_df)

# Count the number of observations in each cluster
cluster_counts = predicted_clusters.groupBy('prediction').count().orderBy('prediction')

# Convert the Spark dataframe to a Pandas dataframe for plotting
pd_df = cluster_counts.toPandas()

# Plot the bar plot of cluster counts
plt.clf()
%matplot plt
fig, ax = plt.subplots()
bars = ax.bar(pd_df['prediction'], pd_df['count'])
plt.xlabel('Cluster')
plt.ylabel('Count')
plt.show()
%matplot plt


# Extract PCA features and predicted cluster labels
pca_features = predicted_clusters.select("pca_features").rdd.map(lambda row: row[0]).collect()
cluster_labels = predicted_clusters.select("prediction").rdd.map(lambda row: row[0]).collect()

# Visualize the clusters using matplotlib
plt.clf()
%matplot plt
x = np.array(pca_features)[:, 0]
y = np.array(pca_features)[:, 1]
plt.scatter(x, y, c=cluster_labels)
plt.show()
%matplot plt

predicted_clusters.show()

predicted_clusters.createOrReplaceTempView("cluster")
cluster_eda = spark.sql("SELECT prediction, CurrentApprovalAmount, JobsReported, BorrowerState_CA, BorrowerState_IL, BorrowerState_FL, BorrowerState_TX, BorrowerState_NY, BusinessType_Corporation, `BusinessType_Limited  Liability Company(LLC)`,`BusinessType_Non-Profit Organization`, `BusinessType_Subchapter S Corporation`,BusinessType_others,`OriginatingLender_Bank of America, National Association`, `OriginatingLender_JPMorgan Chase Bank, National Association`,OriginatingLender_others FROM cluster")

from pyspark.sql.functions import avg,count,round

df = cluster_eda.groupBy("prediction").agg(avg("CurrentApprovalAmount").alias("avg_approval_amount"), 
                             avg("JobsReported").alias("avg_jobs_reported"), 
                             avg(col("BorrowerState_CA")).alias("CA"), 
                             avg(col("BorrowerState_IL")).alias("IL"), 
                             avg(col("BorrowerState_FL")).alias("FL"), 
                             avg(col("BorrowerState_TX")).alias("TX"), 
                             avg(col("BorrowerState_NY")).alias("NY"), 
                             avg(col("BusinessType_Corporation")).alias("Corporation"), 
                             avg(col("BusinessType_Limited  Liability Company(LLC)")).alias("LLC"), 
                             avg(col("BusinessType_Non-Profit Organization")).alias("NGO"), 
                             avg(col("BusinessType_Subchapter S Corporation")).alias("Subchapter_s_corp"), 
                             avg(col("BusinessType_others")).alias("Others"), 
                             avg(col("OriginatingLender_Bank of America, National Association")).alias("Bank of America"), 
                             avg(col("OriginatingLender_JPMorgan Chase Bank, National Association")).alias("JPMorgan"), 
                             avg(col("OriginatingLender_others")).alias("Other_lenders"), 
                             count("*").alias("prediction_count")).orderBy("prediction")
df = df.select([round(c, 4).alias(c) for c in df.columns])
df.show()
