/*
 * This file is part of the PDF Split And Merge source code
 * Created for SWE 261P Software Testing and Analysis
 * Team Member: Kingson
 */
package org.pdfsam.tools.extract;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.mockito.Mockito.mock;
import org.pdfsam.core.support.params.ConversionUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.parameter.ExtractPagesParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PagesSelection;

/**
 * Partition-based tests for ExtractParametersBuilder.
 * 
 * This test class implements systematic partition testing for the PDF extract
 * feature,
 * dividing the input domain into equivalence classes and testing representative
 * values.
 * 
 * @author Kingson
 * @see ExtractParametersBuilder
 */
@DisplayName("Kingson's Extract Partition Tests")
public class KingsonExtractPartitionTest {

    private ExtractParametersBuilder builder;
    private FileOrDirectoryTaskOutput mockOutput;

    @BeforeEach
    void setUp() {
        builder = new ExtractParametersBuilder();
        mockOutput = mock(FileOrDirectoryTaskOutput.class);
        builder.output(mockOutput);
        builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        builder.version(PdfVersion.VERSION_1_7);
        builder.prefix("extracted_");
    }

    /**
     * Partition 1: Page Selection Type Partitions
     */
    @Nested
    @DisplayName("Partition 1: Page Selection Type")
    class PageSelectionTypePartition {

        @Test
        @DisplayName("P1a: Single page number")
        void singlePageNumber(@TempDir Path folder) throws IOException {
            // Partition: Single page selection
            // Representative value: "5"

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("5");
            builder.pagesSelection(selection);

            ExtractPagesParameters params = builder.build();
            assertThat(params.hasPageSelection()).isTrue();
        }

        @Test
        @DisplayName("P1b: Single contiguous range")
        void singleRange(@TempDir Path folder) throws IOException {
            // Partition: Single range of pages
            // Representative value: "1-10"

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-10");
            builder.pagesSelection(selection);

            ExtractPagesParameters params = builder.build();
            assertThat(params.hasPageSelection()).isTrue();
        }

        @Test
        @DisplayName("P1c: Multiple non-contiguous ranges")
        void multipleRanges(@TempDir Path folder) throws IOException {
            // Partition: Multiple separate ranges
            // Representative value: "1-5,10-15,20"

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-5,10-15,20");
            builder.pagesSelection(selection);

            ExtractPagesParameters params = builder.build();
            assertThat(params.hasPageSelection()).isTrue();
        }

        @Test
        @DisplayName("P1d: 'last' keyword for final page")
        void lastKeyword(@TempDir Path folder) throws IOException {
            // Partition: Special 'last' keyword
            // Representative value: "last"

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("last");
            builder.pagesSelection(selection);

            ExtractPagesParameters params = builder.build();
            assertThat(params.hasPageSelection()).isTrue();
        }
    }

    /**
     * Partition 2: Selection Inversion Partitions
     */
    @Nested
    @DisplayName("Partition 2: Selection Inversion")
    class SelectionInversionPartition {

        @Test
        @DisplayName("P2a: Normal selection - extracts specified pages")
        void normalSelection(@TempDir Path folder) throws IOException {
            // Partition: Normal (non-inverted) selection
            // Representative value: invertSelection = false

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-5");
            builder.pagesSelection(selection);
            builder.invertSelection(false);

            ExtractPagesParameters params = builder.build();
            assertThat(params.isInvertSelection()).isFalse();
        }

        @Test
        @DisplayName("P2b: Inverted selection - extracts all EXCEPT specified pages")
        void invertedSelection(@TempDir Path folder) throws IOException {
            // Partition: Inverted selection
            // Representative value: invertSelection = true

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-5");
            builder.pagesSelection(selection);
            builder.invertSelection(true);

            ExtractPagesParameters params = builder.build();
            assertThat(params.isInvertSelection()).isTrue();
        }
    }

    /**
     * Partition 3: Output File Mode Partitions
     */
    @Nested
    @DisplayName("Partition 3: Output File Mode")
    class OutputFileModePartition {

        @Test
        @DisplayName("P3a: Single output file - all pages in one file")
        void singleOutputFile(@TempDir Path folder) throws IOException {
            // Partition: Single output file for all extracted pages
            // Representative value: separateForEachRange = false

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-5,10-15");
            builder.pagesSelection(selection);
            builder.separateForEachRange(false);

            ExtractPagesParameters params = builder.build();
            assertThat(params.isSeparateFileForEachRange()).isFalse();
        }

        @Test
        @DisplayName("P3b: Separate files - one file per range")
        void separateOutputFiles(@TempDir Path folder) throws IOException {
            // Partition: Separate output file for each range
            // Representative value: separateForEachRange = true

            addSampleInput(folder);
            Set<PagesSelection> selection = ConversionUtils.toPagesSelectionSet("1-5,10-15");
            builder.pagesSelection(selection);
            builder.separateForEachRange(true);

            ExtractPagesParameters params = builder.build();
            assertThat(params.isSeparateFileForEachRange()).isTrue();
        }
    }

    // Helper methods
    private PdfFileSource createTempPdfSource(Path folder, String name) throws IOException {
        Path tempFile = Files.createTempFile(folder, name, ".pdf");
        return PdfFileSource.newInstanceNoPassword(tempFile.toFile());
    }

    private void addSampleInput(Path folder) throws IOException {
        builder.addSource(createTempPdfSource(folder, "sample"));
    }
}
