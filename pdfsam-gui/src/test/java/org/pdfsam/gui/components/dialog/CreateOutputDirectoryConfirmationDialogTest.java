/*
 * This file is part of the PDF Split And Merge source code
 * Created on 04 dic 2015
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
package org.pdfsam.gui.components.dialog;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class CreateOutputDirectoryConfirmationDialogTest {
    private boolean confirm = false;
    private Button button;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        CreateOutputDirectoryConfirmationDialog victim = new CreateOutputDirectoryConfirmationDialog(stage);
        button = new Button("show");
        button.setOnAction(a -> confirm = victim.response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        robot.clickOn(button);
        assertTrue(robot.lookup(i18n().tr("The selected output directory does not exist")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Do you want to create it?")).tryQuery().isPresent());
        robot.clickOn(i18n().tr("No"));
    }

    @Test
    public void cancel() {
        this.confirm = true;
        robot.clickOn(button);
        robot.clickOn(i18n().tr("No"));
        assertFalse(this.confirm);
    }

    @Test
    public void overwrite() {
        this.confirm = false;
        robot.clickOn(button);
        robot.clickOn(i18n().tr("Yes"));
        assertTrue(this.confirm);
    }

}
