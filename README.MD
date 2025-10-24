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

## 🤝 Contributing

Contributions are welcome!
If you’d like to add lessons, datasets, or exercises:

1. Fork the repo

2. Create a feature branch

3. Submit a pull request
