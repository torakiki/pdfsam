/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09 feb 2017
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
package org.pdfsam.ui.components.dialog;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.gui.components.dialog.LenientExecutionConfirmationDialog;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class LenientExecutionConfirmationDialogTest {
    private boolean confirm = false;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        LenientExecutionConfirmationDialog victim = new LenientExecutionConfirmationDialog(stage);
        Button button = new Button("show");
        button.setOnAction(a -> confirm = victim.response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        robot.clickOn("show");
        assertTrue(robot.lookup(i18n().tr("PDFsam can try to overcome the failure")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("It may result in PDF files with partial or missing data, proceed anyway?"))
                .tryQuery().isPresent());
        robot.clickOn(i18n().tr("No"));
    }

    @Test
    @Tag("NoHeadless")
    public void no() {
        this.confirm = true;
        robot.clickOn("show");
        robot.clickOn(i18n().tr("No"));
        assertFalse(this.confirm);
    }

    @Test
    @Tag("NoHeadless")
    public void yes() {
        this.confirm = false;
        robot.clickOn("show");
        robot.clickOn(i18n().tr("Yes"));
        assertTrue(this.confirm);
    }
}
