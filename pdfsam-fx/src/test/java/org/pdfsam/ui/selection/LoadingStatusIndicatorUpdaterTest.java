/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.selection;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import javafx.scene.control.Label;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusIndicatorUpdaterTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();

    private Label labeled = new Label();
    private LoadingStatusIndicatorUpdater victim = new LoadingStatusIndicatorUpdater(labeled);

    @Before
    public void setUp() {
        labeled.setText("");
    }

    @Test
    public void textAndTooltip() {
        victim.accept(PdfDescriptorLoadingStatus.ENCRYPTED);
        assertEquals(PdfDescriptorLoadingStatus.ENCRYPTED.getIcon().toString(), labeled.getText());
        assertNotNull(labeled.getTooltip());
    }

    @Test
    public void textAndNoTooltip() {
        victim.accept(PdfDescriptorLoadingStatus.LOADING);
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().toString(), labeled.getText());
        assertNull(labeled.getTooltip());
    }

    @Test
    public void nullSafe() {
        victim.accept(null);
        assertEquals("", labeled.getText());
        assertNull(labeled.getTooltip());
    }

    @Test
    public void nullTextValueFor() {
        assertEquals("", LoadingStatusIndicatorUpdater.textValueFor(null));
    }

    @Test
    public void nullIconTextValueFor() {
        assertEquals("", LoadingStatusIndicatorUpdater.textValueFor(PdfDescriptorLoadingStatus.INITIAL));
    }

    @Test
    public void notNullIconTextValueFor() {
        assertEquals(PdfDescriptorLoadingStatus.REQUESTED.getIcon().toString(),
                LoadingStatusIndicatorUpdater.textValueFor(PdfDescriptorLoadingStatus.REQUESTED));
    }

}
