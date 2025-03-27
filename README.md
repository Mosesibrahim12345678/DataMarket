# DataMarket

A decentralized marketplace for data exchange built on the Stacks blockchain using Clarity smart contracts.

## Overview

DataMarket enables a trustless ecosystem where data providers can monetize their datasets while subscribers can access verified, quality data with automated payments. The platform incorporates staking mechanisms to ensure data quality and provides a rating system for community-driven quality assessment.

## Features

- **Dataset Registration**: Data providers can register datasets with metadata and pricing
- **Staking Mechanism**: Providers stake tokens to ensure data quality and commitment
- **Subscription System**: Users can subscribe to datasets for a fixed period
- **Quality Scoring**: Community-driven rating system for datasets
- **Usage Tracking**: Analytics on dataset access and popularity
- **Automated Payments**: Direct peer-to-peer payments from subscribers to providers

## Smart Contract Architecture 
The DataMarket contract implements the following core functionality:

### Data Structures

- **Datasets**: Stores dataset information including provider, name, description, price, stake amount, quality score, and active status
- **Subscriptions**: Tracks user subscriptions to datasets with expiration timestamps
- **Dataset Ratings**: Records user ratings for datasets they've subscribed to
- **Dataset Usage**: Tracks access statistics for each dataset

### Key Functions

#### For Data Providers

- `register-dataset`: Register a new dataset with required stake
- `deactivate-dataset`: Remove a dataset and reclaim staked tokens

#### For Data Subscribers

- `subscribe-to-dataset`: Purchase access to a dataset
- `rate-dataset`: Provide quality rating for a subscribed dataset

#### Read-Only Functions

- `get-dataset`: Retrieve dataset details
- `has-subscription`: Check if a user has an active subscription
- `get-dataset-usage`: Get usage statistics for a dataset
- `get-provider-datasets`: List all datasets from a specific provider

## Technical Details

### Constants

- Minimum stake requirement: 1 STX (1,000,000 microSTX)
- Default quality score: 50/100
- Subscription duration: 1440 blocks (approximately 10 days on Stacks)

### Error Codes

- `100`: Owner-only function
- `101`: Dataset not found
- `102`: Dataset already exists
- `103`: Insufficient stake
- `104`: Unauthorized access
- `105`: Invalid rating value

## Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) - Clarity development environment
- [Stacks Wallet](https://www.hiro.so/wallet) - For interacting with the deployed contract

### Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/Mosesibrahim12345678/DataMarket.git
   cd DataMarket
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Start the Clarinet console:
   ```bash
   clarinet console
   ```

### Testing the Contract

You can test the contract functions directly in the Clarinet console:

```clarity
;; Register a new dataset
(contract-call? .DataMarket register-dataset "Weather Data 2023" "Historical weather data for major US cities" u100000 u1000000)

;; Subscribe to a dataset
(contract-call? .DataMarket subscribe-to-dataset u1)

;; Rate a dataset
(contract-call? .DataMarket rate-dataset u1 u85)
```

## Use Cases

1. **Research Data Exchange**: Academic institutions can monetize research datasets while ensuring proper attribution
2. **Market Intelligence**: Financial analysts can access high-quality market data with verifiable sources
3. **IoT Data Marketplace**: IoT device networks can monetize sensor data with usage-based pricing
4. **Health Data Sharing**: Anonymized health data can be shared securely with proper compensation

## Future Enhancements

- Advanced quality scoring algorithms using weighted averages
- Revenue sharing mechanisms for data aggregators
- Granular data access control mechanisms
- Dispute resolution system
- Token rewards for high-quality datasets and active participants
- Integration with decentralized storage solutions

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Contact

Project Link: [https://github.com/Mosesibrahim12345678/DataMarket](https://github.com/Mosesibrahim12345678/DataMarket)

---

