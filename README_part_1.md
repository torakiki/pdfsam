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
mvn clean compile

# Package the application
mvn clean package -DskipTests

# Build with tests
mvn clean install -DskipTests
```

> **Note:** The build includes a profile that automatically configures the Java toolchain if running on an older JDK version.

### 2.4 Running the Application

After successful compilation, the application can be run using:

```bash
cd pdfsam-basic
```

```bash
mvn exec:exec
```
```

### 2.5 IDE Setup

For IntelliJ IDEA or Eclipse:

1. Import as Maven project
2. Enable preview features in compiler settings
3. Set Java 21 as the project SDK

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


