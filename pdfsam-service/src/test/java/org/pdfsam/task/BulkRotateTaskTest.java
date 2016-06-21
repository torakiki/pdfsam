/*
 * Created on 21 giu 2016
 * Copyright 2015 by Andrea Vacondio (andrea.vacondio@gmail.com).
 * This file is part of Sejda.
 *
 * Sejda is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sejda is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Sejda.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.task;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;
import org.sejda.core.service.BaseTaskTest;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.rotation.Rotation;
import org.sejda.model.task.Task;

/**
 * @author Andrea Vacondio
 *
 */
public class BulkRotateTaskTest extends BaseTaskTest<BulkRotateParameters> {

    private BulkRotateParameters parameters;

    @Override
    public Task<BulkRotateParameters> getTask() {
        return new BulkRotateTask();
    }

    private void setUpDefaultParameters() {
        parameters = new BulkRotateParameters();
        parameters.addInput(new PdfRotationInput(shortInput(), Rotation.DEGREES_180));
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.OVERWRITE);
    }

    private void setUpParametersWithVersionPrefixAndCompressionSpecified() {
        parameters = new BulkRotateParameters();
        parameters.setCompress(true);
        parameters.setOutputPrefix("test_prefix_");
        parameters.setVersion(PdfVersion.VERSION_1_6);
        parameters.addInput(new PdfRotationInput(shortInput(), Rotation.DEGREES_180));
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.SKIP);
    }

    private void setUpRotateSpecificPages() {
        parameters = new BulkRotateParameters();
        parameters.addInput(new PdfRotationInput(shortInput(), Rotation.DEGREES_90, new PageRange(2, 4)));
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.OVERWRITE);
    }

    private void setUpRotateMultipleInput() {
        parameters = new BulkRotateParameters();
        parameters.addInput(new PdfRotationInput(shortInput(), Rotation.DEGREES_90, new PageRange(2, 4)));
        parameters.addInput(
                new PdfRotationInput(mediumInput(), Rotation.DEGREES_90, new PageRange(2, 4), new PageRange(15, 15)));
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.OVERWRITE);
    }

    private void setUpParametersEncrypted() {
        parameters = new BulkRotateParameters();
        parameters.addInput(new PdfRotationInput(stronglyEncryptedInput(), Rotation.DEGREES_180));
        parameters.setExistingOutputPolicy(ExistingOutputPolicy.OVERWRITE);
    }

    @Test
    public void testExecute() throws IOException {
        setUpDefaultParameters();
        testContext.directoryOutputTo(parameters);
        execute(parameters);
        testContext.assertTaskCompleted();
        testContext.assertCreator().assertPages(4)
                .forEachPdfOutput(d -> d.getPages().forEach(p -> assertEquals(180, p.getRotation())));
    }

    @Test
    public void testRotateSpecificPages() throws IOException {
        setUpRotateSpecificPages();
        testContext.directoryOutputTo(parameters);
        execute(parameters);
        testContext.assertTaskCompleted();
        testContext.assertCreator().assertPages(4).forEachPdfOutput(d -> assertEquals(90, d.getPage(2).getRotation()));
    }

    @Test
    public void testExecuteEncrypted() throws IOException {
        setUpParametersEncrypted();
        testContext.directoryOutputTo(parameters);
        execute(parameters);
        testContext.assertTaskCompleted();
        testContext.assertCreator().assertPages(4)
                .forEachPdfOutput(d -> d.getPages().forEach(p -> assertEquals(180, p.getRotation())));
    }

    @Test
    public void testVersionPrefixAndCreatorAreApplied() throws IOException {
        setUpParametersWithVersionPrefixAndCompressionSpecified();
        testContext.directoryOutputTo(parameters);
        execute(parameters);
        testContext.assertTaskCompleted();
        testContext.assertCreator().assertPages(4).assertVersion(PdfVersion.VERSION_1_6);
    }

    @Test
    public void testMultipleInputOneDoesntContainRange() throws IOException {
        setUpRotateMultipleInput();
        testContext.directoryOutputTo(parameters);
        execute(parameters);
        testContext.assertTaskCompleted();
        testContext.assertOutputSize(2).forEachPdfOutput(d -> {
            assertEquals(90, d.getPage(1).getRotation());
            assertEquals(90, d.getPage(2).getRotation());
            assertEquals(90, d.getPage(3).getRotation());
        });
    }
}
