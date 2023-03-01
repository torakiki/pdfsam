/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.commons;

import javafx.scene.Scene;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.matcher.base.NodeMatchers;
import org.testfx.util.WaitForAsyncUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.pdfsam.ui.components.support.Views.helpIcon;
import static org.testfx.api.FxAssert.verifyThat;
/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class RadioButtonDrivenTextFieldsPaneTest {

    private final ToggleGroup group = new ToggleGroup();
    private RadioButtonDrivenTextFieldsPane victim;

    @Start
    private void start(Stage stage) {
        victim = new RadioButtonDrivenTextFieldsPane(group);
        RadioButton radio = new RadioButton("RADIO");
        TextField field = new TextField();
        field.getStyleClass().add("FIELD");
        field.setText("FIELD");
        victim.addRow(radio, field, helpIcon("Help"));
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void nullRadio() {
        assertThrows(IllegalArgumentException.class, () -> victim.addRow(null, new TextField(), helpIcon("Help")));
    }

    @Test
    public void nullField() {
        assertThrows(IllegalArgumentException.class, () -> victim.addRow(new RadioButton(), null, helpIcon("Help")));

    }

    @Test
    public void nullHelp() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.addRow(new RadioButton(), new TextField(), null));
    }

    @Test
    public void addRow(FxRobot robot) {
        RadioButton radio = robot.lookup("RADIO").queryAs(RadioButton.class);
        assertEquals(group, radio.getToggleGroup());
        verifyThat(".FIELD", NodeMatchers.isDisabled());
    }

    @Test
    public void focusedField(FxRobot robot) {
        TextField field = robot.lookup(".FIELD").queryAs(TextField.class);
        robot.clickOn("RADIO").write("Chuck");
        verifyThat(field, (f) -> "Chuck".equals(f.getText()));
    }
}
