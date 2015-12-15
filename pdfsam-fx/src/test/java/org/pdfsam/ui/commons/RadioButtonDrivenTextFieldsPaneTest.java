/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.loadui.testfx.Assertions.verifyThat;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.ui.help.HelpUtils;

import javafx.scene.Parent;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class RadioButtonDrivenTextFieldsPaneTest extends GuiTest {

    private ToggleGroup group = new ToggleGroup();

    @Override
    protected Parent getRootNode() {
        RadioButtonDrivenTextFieldsPane victim = new RadioButtonDrivenTextFieldsPane(group);
        RadioButton radio = new RadioButton("RADIO");
        TextField field = new TextField();
        field.getStyleClass().add("FIELD");
        field.setText("FIELD");
        victim.addRow(radio, field, HelpUtils.helpIcon("Help"));
        victim.setId("victim");
        return victim;
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullRadio() {
        RadioButtonDrivenTextFieldsPane victim = find("#victim");
        victim.addRow(null, new TextField(), HelpUtils.helpIcon("Help"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullField() {
        RadioButtonDrivenTextFieldsPane victim = find("#victim");
        victim.addRow(new RadioButton(), null, HelpUtils.helpIcon("Help"));
    }

    @Test
    public void nullHelp() throws Exception {
        RadioButtonDrivenTextFieldsPane victim = find("#victim");
        FXTestUtils.invokeAndWait(() -> victim.addRow(new RadioButton(), new TextField(), null), 2);
    }

    @Test
    public void addRow() {
        RadioButton radio = find("RADIO");
        TextField field = find(".FIELD");
        assertEquals(group, radio.getToggleGroup());
        assertTrue(field.isDisable());
    }

    @Test
    public void focusedField() {
        RadioButton radio = find("RADIO");
        TextField field = find(".FIELD");
        click(radio);
        type("Chuck");
        verifyThat(field, (f) -> "Chuck".equals(f.getText()));
    }
}
