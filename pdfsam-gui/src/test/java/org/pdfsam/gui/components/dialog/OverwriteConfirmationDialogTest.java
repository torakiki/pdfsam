/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.gui.components.dialog;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.output.ExistingOutputPolicy;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class OverwriteConfirmationDialogTest {

    private FxRobot robot;
    private Optional<ExistingOutputPolicy> response = Optional.empty();

    @Start
    public void start(Stage stage) {
        OverwriteConfirmationDialog victim = new OverwriteConfirmationDialog(stage);
        Button button = new Button("show");
        button.setOnAction(
                a -> response = victim.title("Title").messageTitle("MessageTitle").messageContent("MessageContent")
                        .buttons(victim.defaultButton("Overwrite", ExistingOutputPolicy.OVERWRITE),
                                victim.button("Rename", ExistingOutputPolicy.RENAME),
                                victim.button("Skip", ExistingOutputPolicy.SKIP), victim.cancelButton("Cancel"))
                        .response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        robot.clickOn("show");
        assertTrue(robot.lookup("MessageTitle").tryQuery().isPresent());
        assertTrue(robot.lookup("MessageContent").tryQuery().isPresent());
        robot.clickOn("Cancel");
    }

    @Test
    @Tag("NoHeadless")
    public void cancel() {
        response = Optional.empty();
        robot.clickOn("show");
        robot.clickOn("Cancel");
        assertTrue(response.isEmpty());
    }

    @Test
    @Tag("NoHeadless")
    public void overwrite() {
        response = Optional.empty();
        robot.clickOn("show");
        robot.clickOn("Overwrite");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.OVERWRITE, response.get());
    }

    @Test
    @Tag("NoHeadless")
    public void skip() {
        response = Optional.empty();
        robot.clickOn("show");
        robot.clickOn("Skip");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.SKIP, response.get());
    }

    @Test
    @Tag("NoHeadless")
    public void rename() {
        response = Optional.empty();
        robot.clickOn("show");
        robot.clickOn("Rename");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.RENAME, response.get());
    }

    @Test
    public void esc() {
        response = Optional.empty();
        robot.clickOn("show");
        robot.push(KeyCode.ESCAPE);
        assertTrue(response.isEmpty());
    }

}
