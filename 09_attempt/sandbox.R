# here i am:
# accommodates * bedrooms - capacity interactions
# review_scores_rating * number_of_reviews - quality-quantity interaction
# neighbourhood_affluence * property_type_encoded - location-property synergy
# availability_30 * instant_bookable - booking convenience factor

# Try quantile-based binning instead of fixed ranges for availability variables
# Create review_recency bins: recent (< 30 days), moderate (30-90 days), old (> 90 days)
# Bin number_of_reviews more granularly: 0, 1-5, 6-15, 16-50, 51-100, 100+

# Create distance-based features if you have lat/lon coordinates
# Generate host_neighbourhood_match (1 if host lives in same area as listing)
# Create neighborhood listing density features
# Add seasonal availability patterns by neighborhood
# 
# Create price-per-person ratios: price/accommodates
# Generate relative pricing: how this listing compares to neighborhood median
# Add value indicators: bathrooms_per_bedroom, beds_per_person

# days_since_last_review = as.numeric(as.Date("2025-03-20") - last_review),
# review_frequency = number_of_reviews / pmax(active_host_since/365, 0.1),
# host_experience_years = existing_host_since / 365

# Instead of just imputing, create missingness indicators:
#   
#   missing_host_response_rate binary flag
# missing_reviews indicator
# These often contain predictive information

# Try sqrt(price + 1) instead of log(price + 1)
# Apply Box-Cox transformation to skewed numeric features
# Use target encoding for high-cardinality categorical variables