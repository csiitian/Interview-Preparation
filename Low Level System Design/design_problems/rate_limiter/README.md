# API Rate Limiter

## Requirements:
1. Each user should be able to make only x requests per y interval
2. Handle burst and spikes

for API rate limiting we have a couple of algorithms
1. Token Bucket
   - Fill Rate
2. Leaky Bucket
   - Leak Rate
3. Fixed Window Counter
   - Maintain a counter for each client per window
   - if count exceeds the limit block requests
4. Sliding Window Counter