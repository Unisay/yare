# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Prerequisites

- Nix package manager with experimental features enabled: `flakes` and `nix-command`
- Enter the development shell: `nix develop`

## Development Commands

All commands below are available inside the Nix development shell. Run `info` to see all available commands.

### Building and Running

- `build` - Build all packages in the project
- `run-offchain` - Run the Yare backend (off-chain) which starts the HTTP server and chain follower
- `run-onchain` - Build and run on-chain Plutus code
- `watch` - Watch for file changes and rebuild automatically
- `dev-run` - Run the project with auto-reload on file changes

### Testing

- `cabal test` - Run the test suite (uses sydtest framework)
- `cabal test yare-test --test-show-details=direct` - Run tests with detailed output

### Code Quality

- `fourmolu -i <file>` - Format a Haskell file (configuration in fourmolu.yaml)
- `cabal-fmt -i <file>` - Format cabal file
- `hlint <file>` - Run linter
- `hooks` - Run all pre-commit hooks (hlint, cabal-fmt)

### Utilities

- `drop-state` - Remove the LMDB database files to reset application state

## Architecture Overview

Yare is a Cardano blockchain application with off-chain and on-chain components.

### Project Structure

The project uses Cabal with two libraries and two executables:

- **Library (`lib/`)**: Main off-chain logic
- **Onchain Library (`onchain/`)**: Plutus smart contracts (compiled with PlutusTx plugin)
- **yare-offchain**: Main backend executable
- **yare-onchain**: Script compilation executable
- **Tests (`test/`)**: Test suite using sydtest

### Core Components

**Application Entry (`Yare.App`)**
The application runs two concurrent threads:
1. HTTP Server serving a RESTful API
2. Node Subscription maintaining a permanent connection to Cardano node with:
   - Chain sync protocol
   - Local state query protocol
   - Local transaction submission protocol

**Chain Follower (`Yare.Chain.Follower`)**
Processes blockchain events:
- `onNewBlock`: Indexes new blocks, updates UTXO set, tracks transaction status
- `onRollback`: Handles chain reorganizations by rolling back state
- Implements finality logic: blocks older than 2160 blocks (security parameter) are considered final
- Switches between volatile and durable storage modes based on finality

**UTXO Management (`Yare.Utxo`)**
- Maintains an indexed set of unspent transaction outputs
- Tracks script deployments and their status
- Supports rollback for chain reorganizations
- Provides queries for spendable entries, balances, and transaction inputs

**Storage Layer (`Yare.Storage`)**
Dual-mode storage system:
- **Volatile storage**: In-memory for fast indexing of recent blocks
- **Durable storage**: LMDB-based persistent storage for finalized blocks
- Automatic mode switching based on block finality
- State snapshots every 10,000 blocks

**HTTP API (`Yare.Http.Server`)**
REST endpoints under `/api`:
- `/utxo` - Query UTXO set and balances
- `/network` - Network info, chain tip, last indexed block
- `/script/{hash}` - Deploy and query Plutus scripts
- `/addresses` - Manage addresses (change, fees, collateral, scripts)
- `/addresses/rebalance` - Rebalance address UTXOs
- `/transactions` - Query submitted and in-ledger transactions
- `/assets/{asset}` - Mint NFTs

**Services (`Yare.App.Services`)**
Business logic layer providing:
- Script deployment
- NFT minting
- Address management and derivation
- Transaction submission
- UTXO queries

### Key Design Patterns

**Type-Level Programming**
- Extensive use of GADTs, DataKinds, TypeFamilies
- `Data.Has` and `Data.HList.Extended` for type-level record access
- Tagged types for disambiguation (e.g., `Tagged "submitted" (Set TxId)`)

**Cardano Integration**
- Built on `cardano-api`, `ouroboros-network`, and `ouroboros-consensus` libraries
- Multi-era support (Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway)
- Uses Cardano Haskell Packages (CHaP) repository

**Error Handling**
- Uses `relude` as custom prelude (NoImplicitPrelude)
- Exception-based error handling with explicit error types

## Code Style

- **Formatter**: Fourmolu with config in `fourmolu.yaml`
  - 2-space indentation
  - 120 character line limit
  - Unicode syntax enabled
  - Leading commas and function arrows
- **Linter**: HLint
- **Extensions**: Many extensions enabled by default in `yare.cabal` common `opts` stanza
- Pre-commit hooks enforce formatting and linting

## Testing

Tests use sydtest framework with automatic test discovery via `sydtest-discover`.
Test modules should be named `*Spec.hs` and placed in `test/` directory.

## Dependencies

- Cardano node socket path and network magic are required for running the off-chain component
- Mnemonic file for address derivation
- LMDB database file for persistent storage
