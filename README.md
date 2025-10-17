# Rental Agreements with Auto-Payment & Late Fee Logic
A decentralized solution for managing rental agreements, automated payments, and late fee calculations on the Stacks blockchain.

## 🌟 Features

- Create and manage rental agreements
- Automated rent collection
- Late fee calculation and enforcement
- Security deposit handling
- Payment history tracking
- Agreement termination with deposit return

## 📝 Contract Functions

### For Landlords

- `create-rental-agreement`: Create a new rental agreement
- `terminate-agreement`: End an active agreement
- `update-rent-amount`: Modify the rent amount

### For Tenants

- `pay-rent`: Submit rent payment
- `get-rental-info`: View agreement details
- `get-payment-history`: Check payment history
- `get-late-fee`: Calculate current late fee

## 🚀 Usage

1. Deploy the contract using Clarinet
2. Create rental agreement as landlord
3. Tenants can make payments
4. Monitor payments and late fees
5. Terminate agreements when needed

## ⚙️ Technical Details

- Late fee: 5% of rent amount
- Grace period: 144 blocks
- Payment tracking with comprehensive history
- Automated security deposit handling

## 🔒 Security

- Owner-only administrative functions
- Secure STX transfer handling
- Agreement state validation
```

