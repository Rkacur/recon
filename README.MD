# 🧮 recon — Learn R for Economics

**recon** (short for **R + Economics**) is an educational project that helps users learn how to use **R** for economic analysis.  
It provides examples, exercises, and best practices for applying data science methods to real-world economic datasets.

---

## 📘 Overview

This repository is intended for:

- Students and researchers new to **R**.
- Economists transitioning from tools like Excel, Stata, or Python.
- Anyone who wants a guided introduction to **data analysis**, **visualization**, and **modeling** in R.

You’ll learn how to:

- Work with data frames, tidy data, and use the `tidyverse` ecosystem.
- Apply statistical and econometric methods.
- Create publication-quality visualizations.
- Reproduce and share results in a reproducible way.

---

## 🧰 Prerequisites

Before getting started, make sure you have:

- **macOS** (other platforms supported with minor modifications)
- **Homebrew** installed — [Installation guide](https://brew.sh)
- **R** and **RStudio** (or VS Code with R extension)

---

## 💻 Installing R with Homebrew

If you don’t already have R installed, you can install it easily with Homebrew:

```bash
# Update Homebrew
brew update

# Install R
brew install r

# Verify installation
R --version
```

Optionally, install RStudio for a more user-friendly IDE:

```bash
brew install --cask rstudio
```

## 📦 Setting Up Your Project Environment with renv

[renv](https://cran.r-project.org/web/packages/renv/index.html) is a dependency management system for R.
<br> It helps ensure everyone working on recon uses the same package versions — critical for reproducibility.

### 1. Initialize renv inside the project

From within your recon directory, start R and run:

```bash
install.packages("renv")
renv::init()
```

This will:

- Create a local renv environment inside the project.
- Snapshot installed packages to renv.lock.

### 2. Restore dependencies (if renv.lock exists)

If you’ve just cloned the repo and a lockfile already exists:

```bash
install.packages("renv")
renv::restore()
```

This ensures all required packages are installed in your isolated environment.

## 🛠 macOS: Configuring Renviron and Makevars

For building packages from source on macOS (especially on Apple Silicon), it can help to explicitly set SDK paths and compiler flags.

1. Create ~/.Renviron

```bash
mkdir -p ~/.Renviron.d
nano ~/.Renviron
```

Add the following:

```bash
# Use the macOS CommandLineTools SDK
SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk

# Homebrew pkg-config path for dependencies
PKG_CONFIG_PATH=/opt/homebrew/opt/libxml2/lib/pkgconfig:/opt/homebrew/opt/icu4c/lib/pkgconfig:/opt/homebrew/opt/gettext/lib/pkgconfig
```

Save ('ctrl' + 'O') and close ('ctrl' + 'X')the file.

2. Create ~/.R/Makevars

```bash
mkdir -p ~/.R
nano ~/.R/Makevars
```

Add the following flags:

```bash
# SDK and compiler
SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
CC=/usr/bin/clang
CXX=/usr/bin/clang++
CXX11=/usr/bin/clang++
CXX14=/usr/bin/clang++
CXX17=/usr/bin/clang++
CXX20=/usr/bin/clang++

CFLAGS=-isysroot $(SDKROOT) -O2 -Wall
CXXFLAGS=-isysroot $(SDKROOT) -O2 -Wall
LDFLAGS=-isysroot $(SDKROOT)

# Homebrew libraries
PKG_CONFIG_PATH=/opt/homebrew/opt/libxml2/lib/pkgconfig:/opt/homebrew/opt/icu4c/lib/pkgconfig:/opt/homebrew/opt/gettext/lib/pkgconfig
CFLAGS += -I/opt/homebrew/include
CPPFLAGS += -I/opt/homebrew/include
CXXFLAGS += -I/opt/homebrew/include
FLIBS=-L/opt/homebrew/opt/gfortran/lib
F77=/opt/homebrew/bin/gfortran
FC=/opt/homebrew/bin/gfortran
```

This ensures R uses the correct compilers, SDK, and Homebrew-installed libraries when building packages from source.

## 💡 Using R with VS Code

To use R within VS Code, follow these steps:

1. Install the R extension:

- Open VS Code
- Go to Extensions (⇧⌘X on macOS)
- Search for R and install the extension by Yuki Ueda (or similar)

2. Set the R executable path:

- Press Cmd+, to open Settings
- Search for R: Rterm Path
- Set the path to your Homebrew R binary, e.g.:

```swift
/opt/homebrew/bin/R
```

3. Install the R Language Server:

- Start R in your terminal or VS Code console:

```r
renv::install("languageserver")
```

This provides code completion, diagnostics, and linting.

4. Optional: Configure VS Code terminal:

- Ensure that your PATH includes Homebrew and your compilers (/opt/homebrew/bin) so that packages can build correctly.

After this setup, you can run R scripts, use the interactive terminal, and benefit from code completions and diagnostics directly in VS Code.

## 🤝 Contributing

Contributions are welcome!
If you’d like to add lessons, datasets, or exercises:

1. Fork the repo

2. Create a feature branch

3. Submit a pull request
