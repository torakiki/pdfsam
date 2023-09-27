/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25 ott 2020
 * Copyright 2019 by Sober Lemur S.r.l. (info@soberlemur.com).
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
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class ClearToolConfirmationDialogTest {
    private Button button;
    private HitTestListener<ClearToolRequest> listener;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @AfterAll
    public static void tearDown() {
        app().clean();
    }

    @Start
    public void start(Stage stage) {
        app().persistentSettings().set(BooleanPersistentProperty.CLEAR_CONFIRMATION, true);
        listener = new HitTestListener<>();
        button = new Button("show");
        new ClearToolConfirmationDialogController(() -> new ClearToolConfirmationDialog(stage));
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShownClearEverything() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest("module", true, true)));
        robot.clickOn("show");
        assertTrue(robot.lookup(i18n().tr("Clear the tool settings")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Do you confirm?")).tryQuery().isPresent());
        robot.clickOn(i18n().tr("No"));
    }

    @Test
    public void contentIsShownDontClearEverything() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest("module", false, true)));
        robot.clickOn("show");
        assertTrue(robot.lookup(i18n().tr("Clear the selection table")).tryQuery().isPresent());
        assertTrue(robot.lookup(i18n().tr("Do you confirm?")).tryQuery().isPresent());
        robot.clickOn(i18n().tr("No"));
    }

    @Test
    public void no() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest("module", false, true)));
        eventStudio().add(ClearToolRequest.class, listener, "module");
        robot.clickOn("show");
        robot.clickOn(i18n().tr("No"));
        assertFalse(listener.isHit());
    }

    @Test
    public void yes() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest("module", false, true)));
        eventStudio().add(ClearToolRequest.class, listener, "module");
        robot.clickOn("show");
        robot.clickOn(i18n().tr("Yes"));
        assertTrue(listener.isHit());
    }

}
