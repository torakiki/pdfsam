# SWE 261P Software Testing and Analysis - Part 1 Report
## PDFsam Basic: Functional Testing and Partitioning

![Java](https://img.shields.io/badge/Java-21-orange?style=for-the-badge&logo=openjdk)
![JUnit 5](https://img.shields.io/badge/JUnit-5-25A162?style=for-the-badge&logo=junit5&logoColor=white)
![Maven](https://img.shields.io/badge/Maven-C71A36?style=for-the-badge&logo=apache-maven&logoColor=white)

<p align="left">
  <img src="https://img.shields.io/badge/Course-SWE_261P-blue?style=flat-square">
  <img src="https://img.shields.io/badge/Quarter-Winter_2026-brightgreen?style=flat-square">
  <img src="https://img.shields.io/badge/Team-Kingson_%26_Zian_%26_Zhenyu-orange?style=flat-square">
</p>

**Team Members:** 
* Kingson Zhang: kxzhang@uci.edu
* Zian Xu: zianx11@uci.edu
* Zhenyu Song: zhenyus4@uci.edu

---

> [!NOTE]
> This report documents the systematic functional testing process of **PDFsam Basic**, focusing on equivalence partitioning across core modules: Merge, Rotate, and Extract.

### 📂 Quick Navigation
- [1. Introduction](#1-introduction) | [2. Build Documentation](#2-build-documentation) | [3. Existing Test Cases](#3-existing-test-cases)
- [4. Partition Testing](#4-partition-testing) | [5. Test Summary](#5-test-implementation-summary) | [6. Conclusion](#6-conclusion)

## 🚀1. Introduction

### 1.1 Repo Introduction

PDFsam (PDF Split And Merge) Basic is a free, open-source, multi-platform desktop application designed to perform various operations on PDF files. PDFsam has become one of the most popular tools for PDF manipulation, offering functionality ranging from simple page extraction to complex document merging operations.

### 1.2 Purpose and Features

PDFsam Basic provides the following core functionalities:

| Feature | Description |
|---------|-------------|
| **Alternate Mix** | Interleave pages from multiple PDF documents |
| **Backpages** | Add backpages to existing PDF documents |
| **Extract** | Extract specific pages or page ranges from PDF documents |
| **Merge** | Combine multiple PDF files into a single document with options for bookmarks, forms, and page normalization |
| **Rotate** | Rotate PDF pages by 90°, 180°, or 270° degrees |
| **Split** | Divide PDFs by page count, size, or bookmarks |

### 1.3 Technical Overview

> [!IMPORTANT]
> **Total Project Scale:** 733 files, **35,994 Logical Lines of Code (LLOC)**, and 15,003 comments.

<details>
<summary>📊Click to View LOC Details</summary>

## Languages
| language | files | code | comment | blank | total |
| :--- | ---: | ---: | ---: | ---: | ---: |
| Java | 621 | 29,387 | 14,692 | 6,064 | 50,143 |
| XML | 45 | 4,087 | 48 | 153 | 4,288 |
| PostCSS | 20 | 1,642 | 152 | 366 | 2,160 |
| Markdown | 3 | 201 | 0 | 118 | 319 |
| YAML | 5 | 176 | 14 | 45 | 235 |
| Java Properties | 22 | 134 | 0 | 4 | 138 |
| Batch | 4 | 107 | 33 | 39 | 179 |
| HTML | 2 | 92 | 2 | 2 | 96 |
| Shell Script | 4 | 85 | 62 | 23 | 170 |
| C# | 1 | 48 | 0 | 3 | 51 |
| JSON | 6 | 35 | 0 | 0 | 35 |

</details>

The project is primarily written in Java (approx. 82%), consisting of roughly 35000 lines of code. [View LOC](./.VSCodeCounter)

### 1.4 Project Architecture

test

---

## 🛠️2. Build Documentation

### 2.1 Prerequisites

Before building PDFsam, ensure the following tools are installed:

- **Java Development Kit (JDK)**: Version 21 or higher
- **Apache Maven**: Version 3.8+ recommended
- **Git**: For cloning the repository

### 2.2 Cloning the Repository

```bash
git clone https://github.com/torakiki/pdfsam.git
cd pdfsam
```

### 2.3 Building the Project

PDFsam uses Java 21's preview features (Foreign Function & Memory API), so the `--enable-preview` flag is required:

```bash
# Compile the project
./mvnw clean compile

# Package the application
./mvnw clean package -DskipTests

# Build with tests
./mvnw clean install
```

> **Note:** The build includes a profile that automatically configures the Java toolchain if running on an older JDK version.

### 2.4 Running the Application

After successful compilation, the application can be run using:

```bash
cd pdfsam-basic
mvn exec:java -Dexec.mainClass="org.pdfsam.basic.App"
```

Alternatively, the packaged JAR can be executed:

```bash
java --enable-preview -jar pdfsam-basic/target/pdfsam-basic-5.4.5-SNAPSHOT.jar
```

### 2.5 IDE Setup

For IntelliJ IDEA or Eclipse:

1. Import as Maven project
2. Enable preview features in compiler settings
3. Set Java 21 as the project SDK

---

## 🧪3. Existing Test Cases

### 3.1 Testing Frameworks

PDFsam employs a comprehensive testing stack:

| Framework | Version | Purpose |
|-----------|---------|---------|
| **JUnit 5 (Jupiter)** | Latest | Unit testing framework |
| **Mockito** | Latest | Mock object creation |
| **AssertJ** | Latest | Fluent assertions |

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

```bash
# Run all tests
mvn test
```

```bash
# Run tests for a specific module
mvn test -pl pdfsam-tools/pdfsam-rotate
```

```bash
# Run a specific test class
cd pdfsam-tools/pdfsam-rotate
mvn test -Dtest=RotateParametersBuilderTest
```

---

## ✨4. Partition Testing

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

**Partition testing** (also known as equivalence partitioning) is a systematic technique that:

1. **Divides the input domain** into partitions where the program is expected to behave equivalently for all values within each partition
2. **Selects representative values** from each partition
3. **Tests boundary values** at partition edges where defects often lurk

**Key principles:**

- **Completeness**: Partitions cover the entire input domain
- **Disjointness**: Partitions don't overlap (each input belongs to exactly one partition)
- **Homogeneity**: All values in a partition should trigger similar behavior

**Benefits of partition testing:**
- Reduces test cases while maintaining effectiveness
- Provides systematic coverage documentation
- Identifies missing test cases
- Focuses testing effort on distinct behaviors

---

### 4.3 Zhenyu's Partition Testing: Merge Feature

#### 4.3.1 Feature Description

The `MergeParametersBuilder` class constructs parameters for PDF merge operations. It handles:
- Input PDF collection management
- Output configuration
- Outline (bookmark) policies
- Form handling policies
- Page normalization
- Table of contents generation

#### 4.3.2 Partitioning Scheme



#### 4.3.3 Test Implementation



---

### 4.4 Zian's Partition Testing: Rotate Feature

#### 4.4.1 Feature Description

The `RotateParametersBuilder` class constructs parameters for PDF page rotation. It handles:
- Rotation angle selection
- Page selection (all, odd, even, or custom ranges)
- Multiple input sources
- Output file naming

#### 4.4.2 Partitioning Scheme

This test suite implements a systematic **Input Domain Partitioning** strategy to validate the `RotateParametersBuilder`. The logic is decomposed into four primary dimensions:

###  Dimension 1: Angular Transformation Mapping
* `[P1a]` Systematic Rotation (90°):** Validates the fundamental mapping of a quadrant clockwise rotation using `Rotation.DEGREES_90`. It ensures that the Builder correctly encapsulates the angular intent into the final task parameters.

###  Dimension 2: Predefined Page Selection Strategy
* `[P1a]` (Identity Mapping):** Verifies the "Rotate All" logic using `PredefinedSetOfPages.ALL_PAGES`. For a standard 10-page document, it asserts that all 10 pages are correctly targeted for transformation.
* `[P2b]` (Odd Parity Filter):** Evaluates the parity-based filtering using `PredefinedSetOfPages.ODD_PAGES`. It confirms that for a 10-page document, only the 5 odd-indexed pages are selected.

###  Dimension 3: Parameter Precedence & Override Logic
* `[P3a]` (Fallback Mechanism):** Confirms the system's "Safe Default" behavior. When no custom ranges are provided (`null`), the system successfully falls back to the global predefined strategy (e.g., Odd Pages).
* `[P3b]` (Complex Range Merging):** Validates the override mechanism using a `HashSet` of multiple disjoint `PageRange` objects (e.g., pages 1-3 and 7-9). It ensures custom user input takes precedence over global settings.

###  Dimension 4: Input Batch Cardinality
* `[P4a]` (Zero Input):** Tests the system's state when no files are added, ensuring `hasInput()` correctly returns `false` to prevent null-pointer operations.
* `[P4b]` (Multi-Source):** Verifies the bulk processing capability by injecting 3 distinct PDF sources. It asserts that the `InputSet` size matches the expected count of 3.

###  Combined Scenario: Integration Verification
* **Cross-Partition Validation:** A composite test case that simultaneously evaluates 90° rotation, multiple input sources with heterogeneous selection strategies (one file with custom ranges, another with predefined sets), and output target consistency.

#### 4.4.3 Test Implementation

The partition tests are implemented in `ZianRotatePartitionTest.java` using JUnit 5, AssertJ, and Mockito.

- **`rotation90Degrees()`**: Covers the *Angular Transformation Partition*. It asserts that when a 90° clockwise rotation is set, the builder correctly maps the `Rotation.DEGREES_90` constant to the resulting task parameters.
- **`allPages()` and `oddPages()`**: Cover the *Predefined Page Set Partition*. These tests simulate a 10-page document and verify that the selection logic correctly calculates the expected page count (e.g., all 10 pages for `ALL_PAGES` vs. 5 pages for `ODD_PAGES`).
- **`noCustomRange()`**: Covers the *Fallback Strategy Partition*. It verifies that if no specific page ranges are provided, the builder successfully defaults to the predefined selection type (e.g., rotating only odd pages).
- **`multipleCustomRanges()`**: Covers the *Custom Override Partition*. It uses a `HashSet` of multiple `PageRange` objects (e.g., pages 1-3 and 7-9) to ensure that explicit user-defined ranges correctly take precedence over global predefined settings.
- **`noInputs()` and `multipleSources()`**: Cover the *Input Cardinality Partition*. These tests establish the system's boundary behavior, asserting that `hasInput()` returns `false` when empty and correctly tracks the size of the input set when multiple PDF sources are injected.
- **`combinedPartitions()`**: Covers the *Integration Scenario*. This comprehensive test validates a complex state where multiple files are processed simultaneously using heterogeneous strategies—one file with a custom range and another using a predefined set—ensuring the builder maintains state integrity across bulk operations.

---

### 4.5 Kingson's Partition Testing: Extract Feature

#### 4.5.1 Feature Description



#### 4.5.2 Partitioning Scheme



#### 4.5.3 Test Implementation



---

## 📋5. Test Implementation Summary

### 5.1 New Test Files

<<<<<<< Updated upstream


### 5.2 Running the Partition Tests


=======
| File | Location | Team Member |
|------|----------|-------------|
| `ZhenyuMergePartitionTest.java` | `pdfsam-tools/pdfsam-merge/src/test/java/org/pdfsam/tools/merge/` | Zhenyu Song |
| `ZianRotatePartitionTest.java` | `pdfsam-tools/pdfsam-rotate/src/test/java/org/pdfsam/tools/rotate/` | Zian Xu |
| `KingsonExtractPartitionTest.java` | `pdfsam-tools/pdfsam-extract/src/test/java/org/pdfsam/tools/extract/` | Kingson Zhang |

### 5.2 Running the Partition Tests

```bash
# Run individual partition tests
mvn test -pl pdfsam-tools/pdfsam-merge -Dtest=ZhenyuMergePartitionTest
mvn test -pl pdfsam-tools/pdfsam-rotate -Dtest=ZianRotatePartitionTest
mvn test -pl pdfsam-tools/pdfsam-extract -Dtest=KingsonExtractPartitionTest
```
>>>>>>> Stashed changes

---

## 🎯6. Conclusion


