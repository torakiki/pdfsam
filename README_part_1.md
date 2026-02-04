# SWE 261P Software Testing and Analysis - Part 1 Report
## PDFsam Basic: Functional Testing and Partitioning

**Team Members:** 
* Kingson Zhang: kxzhang@uci.edu
* Zian Xu: zianx11@uci.edu
* Zhenyu Song: zhenyus4@uci.edu

---

## 1. Introduction

### 1.1 Repo Introduction

PDFsam (PDF Split And Merge) Basic is a free, open-source, multi-platform desktop application designed to perform various operations on PDF files. PDFsam has become one of the most popular tools for PDF manipulation, offering functionality ranging from simple page extraction to complex document merging operations.

### 1.2 Purpose and Features



### 1.3 Technical Overview



### 1.4 Project Architecture



---

## 2. Build Documentation

### 2.1 Prerequisites

Before building PDFsam, ensure the following tools are installed:

- **Java Development Kit (JDK)**: Version 21 (NOT JDK 11)
- **Apache Maven**: Build tool for dependency management
- **Git**: For cloning the repository
- **Gnu gettext**: Required for internationalization
  - **Windows**: Download from [mlocati/gettext-iconv-windows](http://mlocati.github.io/gettext-iconv-windows/)
  - **macOS**: Install via Homebrew (`brew install gettext`) or use Docker
  - **Linux**: Usually pre-installed or available via package manager

### 2.2 Cloning the Repository

```bash
git clone https://github.com/torakiki/pdfsam.git
cd pdfsam
```

### 2.3 Building the Project

PDFsam uses Java 21's preview features (Foreign Function & Memory API), so the `--enable-preview` flag is required:

```bash
# Compile the project
mvn clean compile
```

```bash
# Package the application
mvn clean package -DskipTests
```

```bash
# Build with tests
mvn clean install -DskipTests
```

### 2.4 Running the Application

After successful compilation, the application can be run using:

```bash
cd pdfsam-basic
```

```bash
mvn exec:exec
```

### 2.5 IDE Setup

For IntelliJ IDEA or Eclipse:

1. Import as Maven project
2. Enable preview features in compiler settings
3. Set Java 21 as the project SDK
4. Find and run ./pdfsam-basic/src/main/java/org/pdfsam/basic/App.java

---

## 3. Existing Test Cases

### 3.1 Testing Frameworks



### 3.2 Test Organization

Tests are organized following Maven conventions:

```
src/
├── main/java/        # Production code
└── test/java/        # Test code
    └── org/pdfsam/
        └── tools/
            ├── merge/
            │   ├── MergeParametersBuilderTest.java
            │   ├── MergeOptionsPaneTest.java
            │   └── MergeSelectionPaneTest.java
            ├── rotate/
            │   └── ...
            └── extract/
                └── ...
```

### 3.3 Test Categories



### 3.4 Running Tests



### 3.5 Test Coverage Observations



---

## 4. Partition Testing

### 4.1 Motivation for Systematic Functional Testing

Software testing faces a fundamental challenge: **exhaustive testing is impossible**. For any non-trivial program, the space of possible inputs is effectively infinite. Consider a simple function that takes a 32-bit integer—testing all 4.3 billion possible values is impractical, and real-world inputs are far more complex.

**Systematic functional testing** addresses this by:
- Treating the software as a "black box" based on its specification
- Identifying meaningful categories of inputs
- Ensuring representative coverage of the input domain

This approach is essential because:
1. **Ad-hoc testing** misses edge cases and boundary conditions
2. **Random testing** provides poor coverage of critical scenarios
3. **Developer intuition** often overlooks non-obvious failure modes

### 4.2 Partition Testing Concepts



---

### 4.3 Zhenyu's Partition Testing: Merge Feature

#### 4.3.1 Feature Description

The feature under test is the **PDF Merge Configuration**, specifically the `MergeParametersBuilder` class. This component is responsible for collecting user inputs and settings to construct a valid `MergeParameters` object, which drives the actual merge process. It handles critical configuration options such as:
- Input PDF files and their order.
- Output file destination and overwrite policies.
- Processing options (compression, versioning).
- Content policies (Outline/Bookmarks, Table of Contents, AcroForms handling).
- Page manipulation (normalization, blank pages for odd-numbered files).

#### 4.3.2 Partitioning Scheme

To ensure robust coverage of the configuration logic, the input space was partitioned based on **builder state complexity** and **input validity**:

1.  **Default State Partition**: The builder is used without any explicit configuration.
    -   *Goal*: specific verification of safe defaults.
    -   *Representative Input*: An empty `MergeParametersBuilder` instance.
2.  **Fully Configured Partition**: The builder is provided with explicit, non-default values for every available setting.
    -   *Goal*: Verify that all user choices are correctly captured and propagated.
    -   *Representative Input*: A builder with inputs, `PdfVersion.VERSION_1_6`, `OutlinePolicy.ONE_ENTRY_EACH_DOC`, `ToCPolicy.DOC_TITLES`, etc.
3.  **Input Sequence Partition**: Multiple inputs added in a specific order.
    -   *Goal*: specific verification that the merge order respects the user's input sequence.
    -   *Representative Input*: Inputs `["1.pdf", "2.pdf", "3.pdf"]` added sequentially.
4.  **Redundant/Edge-Case Input Partition**: Duplicate or redundant inputs.
    -   *Goal*: specific verification of deduplication logic.
    -   *Representative Input*: The same `PdfMergeInput` object added twice.
5.  **Invalid Input Partition**: Null or missing values.
    -   *Goal*: Ensure null safety and robustness.
    -   *Representative Input*: `addInput(null)` and setting policies to `null`.

#### 4.3.3 Test Implementation

The partition tests are implemented in `ZhenyuMergePartitionTest.java` using JUnit 5 and Mockito.

-   **`testDefaults()`**: Covers the *Default State Partition*. Asserts that a fresh builder produces parameters with expected defaults (e.g., `OutlinePolicy.RETAIN`, `ToCPolicy.NONE`, `isCompress` false).
-   **`testFullConfiguration()`**: Covers the *Fully Configured Partition*. Sets every property (e.g., `compress(true)`, `version(1.6)`) and asserts the resulting `MergeParameters` object reflects these exact values.
-   **`testAddInput_PreservesOrder()`**: Covers the *Input Sequence Partition*. Adds three mock inputs and verifies they appear in the exact same order in the final list.
-   **`testAddInput_Deduplicates()`**: Covers *Redundant Input Partition*. Adds the same input object twice and asserts the list size is 1.
-   **`testAddInput_IgnoresNull()`** and **`testNullPolicies_AreAllowed()`**: Covers the *Invalid Input Partition*. Verifies that adding `null` inputs serves no operation and that setting null policies doesn't crash the builder.

---

### 4.4 Zian's Partition Testing: Rotate Feature

#### 4.4.1 Feature Description



#### 4.4.2 Partitioning Scheme


#### 4.4.3 Test Implementation



---

### 4.5 Kingson's Partition Testing: Extract Feature

#### 4.5.1 Feature Description



#### 4.5.2 Partitioning Scheme



#### 4.5.3 Test Implementation



---

## 5. Test Implementation Summary

### 5.1 New Test Files

| File | Location | Team Member |
|------|----------|-------------|
| `ZhenyuMergePartitionTest.java` | `pdfsam-tools/pdfsam-merge/src/test/java/org/pdfsam/tools/merge/` | Zhenyu Song |
| `KarryRotatePartitionTest.java` | `pdfsam-tools/pdfsam-rotate/src/test/java/org/pdfsam/tools/rotate/` | Zian Zhang |
| `KingsonExtractPartitionTest.java` | `pdfsam-tools/pdfsam-extract/src/test/java/org/pdfsam/tools/extract/` | Kingson Zhang |

### 5.2 Running the Partition Tests

```bash
# Run individual partition tests
mvn test -pl pdfsam-tools/pdfsam-merge -Dtest=ZhenyuMergePartitionTest
mvn test -pl pdfsam-tools/pdfsam-rotate -Dtest=KarryRotatePartitionTest
mvn test -pl pdfsam-tools/pdfsam-extract -Dtest=KingsonExtractPartitionTest
```

---

## 6. Conclusion


