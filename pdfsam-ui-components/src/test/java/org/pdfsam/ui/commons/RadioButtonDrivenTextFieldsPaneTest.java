/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.testfx.api.FxAssert.verifyThat;

import org.junit.Test;
import org.pdfsam.ui.help.HelpUtils;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class RadioButtonDrivenTextFieldsPaneTest extends ApplicationTest {

    private ToggleGroup group = new ToggleGroup();
    private RadioButtonDrivenTextFieldsPane victim;

    @Override
    public void start(Stage stage) {
        victim = new RadioButtonDrivenTextFieldsPane(group);
        RadioButton radio = new RadioButton("RADIO");
        TextField field = new TextField();
        field.getStyleClass().add("FIELD");
        field.setText("FIELD");
        victim.addRow(radio, field, HelpUtils.helpIcon("Help"));
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullRadio() {
        victim.addRow(null, new TextField(), HelpUtils.helpIcon("Help"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullField() {
        victim.addRow(new RadioButton(), null, HelpUtils.helpIcon("Help"));
    }

    @Test
    public void nullHelp() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.addRow(new RadioButton(), new TextField(), null));
    }

    @Test
    public void addRow() {
        RadioButton radio = lookup("RADIO").queryAs(RadioButton.class);
        TextField field = lookup(".FIELD").queryAs(TextField.class);
        assertEquals(group, radio.getToggleGroup());
        assertTrue(field.isDisable());
    }

    @Test
    public void focusedField() {
        RadioButton radio = lookup("RADIO").queryAs(RadioButton.class);
        TextField field = lookup(".FIELD").queryAs(TextField.class);
        clickOn(radio).write("Chuck");
        verifyThat(field, (f) -> "Chuck".equals(f.getText()));
    }
}
