/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.components.io;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;
import org.testfx.api.FxAssert;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class BrowsableFileFieldUITest {

    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        var victimBlank = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victimBlank.enforceValidation(false, true);
        victimBlank.getStyleClass().add("victim-blank");

        var victimNoBlank = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victimNoBlank.enforceValidation(false, false);
        victimNoBlank.getStyleClass().add("victim-no-blank");
        Scene scene = new Scene(new HBox(victimBlank, victimNoBlank));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validBlank() {
        BrowsableFileField victim = robot.lookup(".victim-blank").queryAs(BrowsableFileField.class);
        robot.clickOn(victim).type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.VALID);
    }

    @Test
    public void invalidBlank() {
        BrowsableFileField victim = robot.lookup(".victim-no-blank").queryAs(BrowsableFileField.class);
        robot.clickOn(victim).type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.INVALID);
        Arrays.stream(Style.INVALID.css()).forEach(c -> assertTrue(robot.lookup("." + c).tryQuery().isPresent()));
    }
}
