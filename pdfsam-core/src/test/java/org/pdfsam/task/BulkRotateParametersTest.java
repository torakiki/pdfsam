/*
 * Created on 21 giu 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import static org.mockito.Mockito.mock;

import org.junit.Test;
import org.pdfsam.TestUtils;
import org.sejda.model.input.PdfSource;
import org.sejda.model.rotation.Rotation;

/**
 * @author Andrea Vacondio
 *
 */
public class BulkRotateParametersTest {
    @Test
    public void testEquals() {
        PdfSource<?> source = mock(PdfSource.class);
        PdfRotationInput one = new PdfRotationInput(source, Rotation.DEGREES_180);
        PdfRotationInput two = new PdfRotationInput(source, Rotation.DEGREES_90);
        BulkRotateParameters eq1 = new BulkRotateParameters();
        eq1.addInput(one);
        BulkRotateParameters eq2 = new BulkRotateParameters();
        eq2.addInput(one);
        BulkRotateParameters eq3 = new BulkRotateParameters();
        eq3.addInput(one);
        BulkRotateParameters diff = new BulkRotateParameters();
        diff.addInput(two);
        TestUtils.testEqualsAndHashCodes(eq1, eq2, eq3, diff);
    }
}
