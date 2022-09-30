/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.components.selection;

import javafx.scene.control.Label;
import javafx.scene.text.Text;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(JavaFxThreadInitializeExtension.class)
public class LoadingStatusIndicatorUpdaterTest {

    private Label labeled = new Label();
    private LoadingStatusIndicatorUpdater victim = new LoadingStatusIndicatorUpdater(labeled);

    @BeforeEach
    public void setUp() {
        labeled.setText("");
    }

    @Test
    public void textAndTooltipAndStyle() {
        victim.accept(PdfDescriptorLoadingStatus.ENCRYPTED);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.ENCRYPTED.getIcon().getCode()),
                ((Text) labeled.getGraphic()).getText());
        assertTrue(labeled.getStyleClass().contains(PdfDescriptorLoadingStatus.ENCRYPTED.getStyle()));
        assertNotNull(labeled.getTooltip());
    }

    @Test
    public void styleIsRemoved() {
        victim.accept(PdfDescriptorLoadingStatus.ENCRYPTED);
        assertTrue(labeled.getStyleClass().contains(PdfDescriptorLoadingStatus.ENCRYPTED.getStyle()));
        victim.accept(PdfDescriptorLoadingStatus.REQUESTED);
        assertFalse(labeled.getStyleClass().contains(PdfDescriptorLoadingStatus.ENCRYPTED.getStyle()));
    }

    @Test
    public void textAndNoTooltip() {
        victim.accept(PdfDescriptorLoadingStatus.LOADING);
        assertEquals(String.valueOf((char) PdfDescriptorLoadingStatus.LOADING.getIcon().getCode()),
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
