package org.pdfsam.tools.merge;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfMergeInput;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.scale.PageNormalizationPolicy;
import org.sejda.model.toc.ToCPolicy;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

/**
 * @author Zhenyu Song
 * @see MergeParametersBuilder
 */
@DisplayName("Zhenyu's Merge Partition Tests")
public class ZhenyuMergePartitionTest {

    private MergeParametersBuilder builder;
    private FileTaskOutput mockOutput;

    @BeforeEach
    void setUp() {
        builder = new MergeParametersBuilder();
        mockOutput = mock(FileTaskOutput.class);
        builder.output(mockOutput);
    }

    private PdfMergeInput createSampleInput(Path folder, String name) throws IOException {
        Path tempFile = Files.createTempFile(folder, name, ".pdf");
        return new PdfMergeInput(PdfFileSource.newInstanceNoPassword(tempFile.toFile()));
    }

    @Test
    @DisplayName("Defaults: Verifies builder creates parameters with expected default values")
    void testDefaults() {
        MergeParameters params = builder.build();

        assertThat(params.getInputList()).isEmpty();
        assertThat(params.getOutlinePolicy()).isEqualTo(OutlinePolicy.RETAIN);
        assertThat(params.getTableOfContentsPolicy()).isEqualTo(ToCPolicy.NONE);
        assertThat(params.getPageNormalizationPolicy()).isEqualTo(PageNormalizationPolicy.NONE);
        assertThat(params.getAcroFormPolicy()).isEqualTo(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS);
        assertThat(params.isBlankPageIfOdd()).isFalse();
        assertThat(params.isFilenameFooter()).isFalse();
        assertThat(params.getExistingOutputPolicy()).isEqualTo(ExistingOutputPolicy.RENAME);
    }

    @Test
    @DisplayName("Full Config: Verifies a completely customized configuration")
    void testFullConfiguration(@TempDir Path folder) throws IOException {
        PdfMergeInput input = createSampleInput(folder, "custom");

        builder.addInput(input);
        builder.version(PdfVersion.VERSION_1_6);
        builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        builder.compress(true);
        builder.outlinePolicy(OutlinePolicy.ONE_ENTRY_EACH_DOC);
        builder.tocPolicy(ToCPolicy.DOC_TITLES);
        builder.pageNormalizationPolicy(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED);
        builder.acroFormsPolicy(AcroFormPolicy.FLATTEN);
        builder.blankPageIfOdd(true);
        builder.footer(true);

        MergeParameters params = builder.build();

        assertThat(params.getInputList()).containsExactly(input);
        assertThat(params.getVersion()).isEqualTo(PdfVersion.VERSION_1_6);
        assertThat(params.getExistingOutputPolicy()).isEqualTo(ExistingOutputPolicy.OVERWRITE);
        assertThat(params.isCompress()).isTrue();
        assertThat(params.getOutlinePolicy()).isEqualTo(OutlinePolicy.ONE_ENTRY_EACH_DOC);
        assertThat(params.getTableOfContentsPolicy()).isEqualTo(ToCPolicy.DOC_TITLES);
        assertThat(params.getPageNormalizationPolicy()).isEqualTo(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED);
        assertThat(params.getAcroFormPolicy()).isEqualTo(AcroFormPolicy.FLATTEN);
        assertThat(params.isBlankPageIfOdd()).isTrue();
        assertThat(params.isFilenameFooter()).isTrue();
    }

    @Test
    @DisplayName("Ordering: Verifies that inputs are preserved in the order they are added")
    void testAddInput_PreservesOrder(@TempDir Path folder) throws IOException {
        PdfMergeInput input1 = createSampleInput(folder, "1");
        PdfMergeInput input2 = createSampleInput(folder, "2");
        PdfMergeInput input3 = createSampleInput(folder, "3");

        builder.addInput(input1);
        builder.addInput(input2);
        builder.addInput(input3);

        MergeParameters params = builder.build();
        assertThat(params.getInputList())
                .containsExactly(input1, input2, input3);
    }

    @Test
    @DisplayName("Deduplication: Verifies that adding the same input instance twice results in a single entry")
    void testAddInput_Deduplicates(@TempDir Path folder) throws IOException {
        PdfMergeInput input1 = createSampleInput(folder, "1");

        builder.addInput(input1);
        builder.addInput(input1);

        MergeParameters params = builder.build();
        assertThat(params.getInputList())
                .hasSize(1)
                .containsExactly(input1);
    }

    @Test
    @DisplayName("Null Safety: Verifies that adding null input is ignored")
    void testAddInput_IgnoresNull() {
        builder.addInput(null);
        assertThat(builder.hasInput()).isFalse();
        assertThat(builder.build().getInputList()).isEmpty();
    }

    @Test
    @DisplayName("Robustness: Verifies behavior when policies are explicitly set to null")
    void testNullPolicies_AreAllowed() {
        builder.outlinePolicy(null);
        builder.tocPolicy(null);
        builder.pageNormalizationPolicy(null);
        builder.acroFormsPolicy(null);

        MergeParameters params = builder.build();

        assertThat(params.getOutlinePolicy()).isNull();
        assertThat(params.getTableOfContentsPolicy()).isEqualTo(ToCPolicy.NONE);
        assertThat(params.getPageNormalizationPolicy()).isNull();
        assertThat(params.getAcroFormPolicy()).isNull();
    }
}
