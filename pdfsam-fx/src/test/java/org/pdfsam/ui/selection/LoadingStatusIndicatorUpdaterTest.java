/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

import javafx.scene.control.Label;
import javafx.scene.text.Text;

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
        assertEquals(PdfDescriptorLoadingStatus.ENCRYPTED.getIcon().characterToString(),
                ((Text) labeled.getGraphic()).getText());
        assertNotNull(labeled.getTooltip());
    }

    @Test
    public void textAndNoTooltip() {
        victim.accept(PdfDescriptorLoadingStatus.LOADING);
        assertEquals(PdfDescriptorLoadingStatus.LOADING.getIcon().characterToString(),
                ((Text) labeled.getGraphic()).getText());
        assertNull(labeled.getTooltip());
    }

    @Test
    public void nullSafe() {
        victim.accept(null);
        assertEquals("", labeled.getText());
        assertNull(labeled.getTooltip());
    }

    @Test
    public void nullState() {
        victim.accept(null);
        assertNull(labeled.getGraphic());
    }

    @Test
    public void nullIcon() {
        victim.accept(PdfDescriptorLoadingStatus.INITIAL);
        assertNull(labeled.getGraphic());
    }

    @Test
    public void notNullIcon() {
        victim.accept(PdfDescriptorLoadingStatus.REQUESTED);
        assertNotNull(labeled.getGraphic());
    }

}
