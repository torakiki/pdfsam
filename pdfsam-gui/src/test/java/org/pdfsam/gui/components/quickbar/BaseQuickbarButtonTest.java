/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ago/2014
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
package org.pdfsam.gui.components.quickbar;

import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Label;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ JavaFxThreadInitializeExtension.class })
public class BaseQuickbarButtonTest {

    @Test
    public void contentDisplay() {
        BaseQuickbarButton victim = new BaseQuickbarButton();
        assertFalse(victim.isDisplayText());
        assertEquals(ContentDisplay.GRAPHIC_ONLY, victim.getContentDisplay());
        victim.setDisplayText(true);
        assertTrue(victim.isDisplayText());
        assertEquals(ContentDisplay.LEFT, victim.getContentDisplay());
    }

    @Test
    public void selected() {
        BaseQuickbarButton victim = new BaseQuickbarButton();
        assertFalse(victim.isSelected());
        assertFalse(victim.isDisabled());
        victim.setSelected(true);
        assertTrue(victim.isSelected());
        assertTrue(victim.isDisabled());
    }

    @Test
    public void graphicClass() {
        BaseQuickbarButton victim = new BaseQuickbarButton();
        Label graphic = new Label();
        assertFalse(graphic.getStyleClass().contains("ikonli-font-icon"));
        victim.setGraphic(graphic);
        assertTrue(graphic.getStyleClass().contains("ikonli-font-icon"));
    }

    @Test
    public void nullProofGraphicListener() {
        BaseQuickbarButton victim = new BaseQuickbarButton();
        victim.setGraphic(null);
    }

}
