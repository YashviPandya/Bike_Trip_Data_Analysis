# 🚴‍♀️ Cyclistic Bike-Share Data Analysis

## 📍 Project Overview
This project explores two years of trip data from **Cyclistic**, a fictional bike-share company. The main objective is to analyze ride patterns and **compare behavior between annual members and casual riders**, using R for data cleaning and Tableau for data visualization.

This case study was developed as part of my data analytics learning journey and focuses on real-world, actionable insights that can guide strategic marketing decisions for customer conversion and engagement.

---

## 📂 Dataset Summary
The dataset contains:
- Rider type (member or casual)
- Ride duration and distance
- Trip day and time
- Station names and IDs
- Weekday/weekend classifications

Data Source: Provided by Cyclistic's public datasets (simulated for case study)

---

## 🧹 Data Cleaning Process (in R)
Performed in R, the cleaning steps included:
- ✅ Removing null or duplicate entries
- ✅ Standardizing date-time formats
- ✅ Filtering out outliers (e.g., ride duration over 24 hours)
- ✅ Renaming columns for consistency
- ✅ Creating new features like `day_of_week`, `ride_length`, and `user_type`

*All steps are documented in the R script included in this repo.*

---

## 📊 Key Visualizations (Tableau Dashboard)
- 📈 Average ride duration by rider type
- 📅 Weekly usage patterns by rider type
- 🔄 Ride trends by weekday/weekend
- 🗓️ Peak usage days for casual riders

*Dashboard built using Tableau and available in `.twbx` format.*

---

## 💡 Key Findings
- **Casual riders** have **longer average ride durations** than members
- Most casual rides occur on **Thursdays and Fridays**
- **Members show consistent usage across weekdays**, indicating commuting behavior

---

## 🎯 Business Recommendations
Based on the analysis:
- 🎁 Introduce a **free trial ride (0.5 km)** to convert casual riders into members
- 📅 Launch **membership promotions on Thursdays and Fridays** when casual rider traffic is highest
- 🛒 Offer targeted discounts to casual riders with frequent long-distance trips

---

## 🚀 Project Impact
The analysis supports **data-driven marketing decisions** for customer acquisition. Implementing these strategies can increase **conversion rates**, boost **revenue**, and improve **rider engagement**.

---

## 📁 Files Included
- `Cyclistic_Analysis.R`: Full R script for data cleaning and analysis
- `Cyclistic_Dashboard.twbx`: Tableau dashboard file
- `CaseStudy.pptx`: Business-facing summary for non-technical stakeholders
- `README.md`: This file

---

## 👩‍💻 About Me
I'm **Yashvi Pandya**, a London-based Data Analyst with an MSc in Management with Data Analytics. I’m passionate about building actionable insights from data and turning patterns into strategy.

📫 Connect on [LinkedIn](https://www.linkedin.com/in/yashvipandya)  
🔗 Explore more projects on [GitHub](https://github.com/YashviPandya)
