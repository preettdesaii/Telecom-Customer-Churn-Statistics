# Telecom_Customer_Churn

## Executive Summary
The telecom industry faces fierce competition, prompting companies to focus on retaining customers by predicting churn. This project utilizes machine learning models trained on consumer data to predict customer churn. The dataset, sourced from IBM, includes features such as consumer subscriptions, charges, tenure, demographics, and products used. Various machine learning models are trained and evaluated, with the goal of improving customer retention strategies.

## Project Motivation
Customer retention is crucial for telecom companies, as acquiring new customers is more expensive than retaining existing ones. By predicting churn, companies can take proactive measures to retain customers, thereby enhancing customer satisfaction and increasing revenue. This project aims to develop a data-driven solution to lower churn rates.

## Data Source
The dataset used in this project is sourced from IBM and consists of 7043 data points and 33 features. It includes demographic data, account information, services data, and customer satisfaction scores. Dimension reduction techniques are applied to manage the dataset effectively.

[Link to the dataset](insert_dataset_link_here)

## Data Description
### Demographics
- CustomerID: Unique identifier for each customer
- Gender: Customer's gender (Male, Female)
- Age: Customer's current age
- Senior Citizen: Indicates if the customer is 65 or older
- Married: Indicates if the customer is married
- Dependents: Indicates if the customer lives with any dependents
- Number of Dependents: Number of dependents living with the customer

### Location
- Country: Customer's primary residence country
- State: Customer's primary residence state
- City: Customer's primary residence city
- Zip Code: Customer's primary residence zip code
- Latitude: Latitude of the customer's primary residence
- Longitude: Longitude of the customer's primary residence

### Services
- Quarter: Fiscal quarter of the data
- Referred a Friend: Indicates if the customer referred someone to the company
- Number of Referrals: Number of referrals made by the customer
- Tenure in Months: Total months the customer has been with the company
- Offer: Last marketing offer accepted by the customer
- Phone Service: Indicates if the customer subscribes to home phone service
- Internet Service: Type of internet service subscribed by the customer
- Avg Monthly GB Download: Average monthly download volume in gigabytes
- Contract: Customer's current contract type
- Paperless Billing: Indicates if the customer opted for paperless billing
- Payment Method: Customer's bill payment method

### Status
- Quarter: Fiscal quarter of the data
- Satisfaction Score: Customer's satisfaction rating
- Customer Status: Customer's status at the end of the quarter (Churned, Stayed, Joined)
- Churn Label: Indicates if the customer churned
- Churn Score: Predicted churn score
- CLTV: Predicted Customer Lifetime Value
- Churn Category: High-level category for churn reason
- Churn Reason: Specific reason for churn

## Project Scope
The project involves training and evaluating machine learning models to predict customer churn using demographic, account, and services data. The scope can be expanded to include additional features and data points for industrial applications.
