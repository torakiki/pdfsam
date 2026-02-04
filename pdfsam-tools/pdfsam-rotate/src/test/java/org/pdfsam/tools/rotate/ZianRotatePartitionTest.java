package org.pdfsam.tools.rotate;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.task.BulkRotateParameters;
import org.pdfsam.model.task.PdfRotationInput;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

public class ZianRotatePartitionTest {

    private RotateParametersBuilder builder;
    private FileOrDirectoryTaskOutput mockOutput;

    @BeforeEach
    void setUp() {
        builder = new RotateParametersBuilder();
        mockOutput = mock(FileOrDirectoryTaskOutput.class);
        builder.output(mockOutput);
        builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        builder.version(PdfVersion.VERSION_1_7);
        builder.prefix("rotated_");
    }

    @Nested
    @DisplayName("Partition 1: Rotation Angle")
    class RotationAnglePartition {

        @Test
        @DisplayName("90 degrees clockwise")
        void rotation90Degrees(@TempDir Path folder) throws IOException {
            // Partition: Quarter turn clockwise
            // Representative value: Rotation.DEGREES_90

            builder.rotation(Rotation.DEGREES_90);
            builder.rotationType(PredefinedSetOfPages.ALL_PAGES);
            addSampleInput(folder, null);

            BulkRotateParameters params = builder.build();
            Set<PdfRotationInput> inputs = params.getInputSet();

            assertThat(inputs).hasSize(1);
            PdfRotationInput input = inputs.iterator().next();
            assertThat(input.rotation).isEqualTo(Rotation.DEGREES_90);
        }
    }

    @Nested
    @DisplayName("Partition 2: Predefined Page Set")
    class PredefinedPageSetPartition {

        @Test
        @DisplayName("rotates entire document")
        void allPages(@TempDir Path folder) throws IOException {
            // Partition: All pages in document
            // Representative value: PredefinedSetOfPages.ALL_PAGES

            builder.rotation(Rotation.DEGREES_90);
            builder.rotationType(PredefinedSetOfPages.ALL_PAGES);
            addSampleInput(folder, null);

            BulkRotateParameters params = builder.build();
            PdfRotationInput input = params.getInputSet().iterator().next();

            // For a 10-page document, all pages should be selected
            assertThat(input.getPages(10)).hasSize(10);
        }

        @Test
        @DisplayName("rotates pages 1, 3, 5, ...")
        void oddPages(@TempDir Path folder) throws IOException {
            // Partition: Odd-numbered pages only
            // Representative value: PredefinedSetOfPages.ODD_PAGES

            builder.rotation(Rotation.DEGREES_180);
            builder.rotationType(PredefinedSetOfPages.ODD_PAGES);
            addSampleInput(folder, null);

            BulkRotateParameters params = builder.build();
            PdfRotationInput input = params.getInputSet().iterator().next();

            // For a 10-page document, odd pages are 1,3,5,7,9 = 5 pages
            assertThat(input.getPages(10)).hasSize(5);
        }
    }

    @Nested
    @DisplayName("Partition 3: Custom Page Range")
    class CustomRangePartition {

        @Test
        @DisplayName("No custom range")
        void noCustomRange(@TempDir Path folder) throws IOException {

            builder.rotation(Rotation.DEGREES_90);
            builder.rotationType(PredefinedSetOfPages.ODD_PAGES);
            addSampleInput(folder, null);

            BulkRotateParameters params = builder.build();
            PdfRotationInput input = params.getInputSet().iterator().next();

            // Should use predefined set (odd pages)
            Set<Integer> pages = input.getPages(6);
            assertThat(pages).containsExactlyInAnyOrder(1, 3, 5);
        }

        @Test
        @DisplayName("Custom ranges")
        void multipleCustomRanges(@TempDir Path folder) throws IOException {
            // Partition: Multiple page ranges specified
            // Representative value: [PageRange(1, 3), PageRange(7, 9)]

            builder.rotation(Rotation.DEGREES_90);
            builder.rotationType(PredefinedSetOfPages.ALL_PAGES);

            Set<PageRange> ranges = new HashSet<>();
            ranges.add(new PageRange(1, 3));
            ranges.add(new PageRange(7, 9));
            addSampleInput(folder, ranges);

            BulkRotateParameters params = builder.build();
            PdfRotationInput input = params.getInputSet().iterator().next();

            // Should select pages 1,2,3,7,8,9 = 6 pages
            assertThat(input.getPages(10)).hasSize(6);
        }
    }

    @Nested
    @DisplayName("Partition 4: Input Source Count")
    class InputSourceCountPartition {

        @Test
        @DisplayName("No inputs")
        void noInputs() {
            builder.rotation(Rotation.DEGREES_90);
            builder.rotationType(PredefinedSetOfPages.ALL_PAGES);

            assertThat(builder.hasInput()).isFalse();
        }

        @Test
        @DisplayName("Multiple sources")
        void multipleSources(@TempDir Path folder) throws IOException {

            builder.rotation(Rotation.DEGREES_180);
            builder.rotationType(PredefinedSetOfPages.ODD_PAGES);

            for (int i = 0; i < 3; i++) {
                PdfFileSource source = createTempPdfSource(folder, "file" + i);
                builder.addInput(source, null);
            }

            assertThat(builder.hasInput()).isTrue();

            BulkRotateParameters params = builder.build();
            assertThat(params.getInputSet()).hasSize(3);
        }
    }

    @Test
    @DisplayName("Combined: Multiple partitions with representative values")
    void combinedPartitions(@TempDir Path folder) throws IOException {
        // Combine partitions: 90° rotation, custom range, multiple sources
        builder.rotation(Rotation.DEGREES_90);
        builder.rotationType(PredefinedSetOfPages.ALL_PAGES);

        // Add first source with custom range
        PdfFileSource source1 = createTempPdfSource(folder, "doc1");
        Set<PageRange> ranges = new HashSet<>();
        ranges.add(new PageRange(1, 5));
        builder.addInput(source1, ranges);

        // Add second source with predefined set
        PdfFileSource source2 = createTempPdfSource(folder, "doc2");
        builder.addInput(source2, null); // Uses predefined

        BulkRotateParameters params = builder.build();

        assertThat(params.getInputSet()).hasSize(2);
        assertThat(params.getOutput()).isEqualTo(mockOutput);
    }


    private PdfFileSource createTempPdfSource(Path folder, String name) throws IOException {
        Path tempFile = Files.createTempFile(folder, name, ".pdf");
        return PdfFileSource.newInstanceNoPassword(tempFile.toFile());
    }

    private void addSampleInput(Path folder, Set<PageRange> ranges) throws IOException {
        PdfFileSource source = createTempPdfSource(folder, "sample");
        builder.addInput(source, ranges);
    }
}
