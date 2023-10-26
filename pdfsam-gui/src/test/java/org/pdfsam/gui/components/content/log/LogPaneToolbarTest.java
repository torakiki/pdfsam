package org.pdfsam.gui.components.content.log;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Isolated;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.log.ClearLogRequest;
import org.pdfsam.model.log.SaveLogRequest;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxAssert;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.framework.junit5.Stop;
import org.testfx.matcher.base.NodeMatchers;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.testfx.util.WaitForAsyncUtils.waitForFxEvents;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Isolated
class LogPaneToolbarTest {

    private LogPaneToolbar victim;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        victim = new LogPaneToolbar(new LogListView());
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Stop
    public void stop() {
        app().runtimeState().activeTool(null);
    }

    @Test
    public void onClickSave() {
        var button = robot.lookup(i18n().tr("_Save")).queryAs(LogPaneToolbar.SaveButton.class);
        assertTrue(button.isDisabled());
        eventStudio().broadcast(new LogMessage("test", LogLevel.INFO));
        waitForFxEvents();
        assertFalse(button.isDisabled());
        HitTestListener<SaveLogRequest> listener = new HitTestListener<>();
        eventStudio().add(SaveLogRequest.class, listener);
        robot.clickOn(button);
        assertTrue(listener.isHit());
    }

    @Test
    public void onClickClear() {
        var button = robot.lookup(i18n().tr("_Clear")).queryAs(LogPaneToolbar.ClearButton.class);
        assertTrue(button.isDisabled());
        eventStudio().broadcast(new LogMessage("test", LogLevel.INFO));
        waitForFxEvents();
        assertFalse(button.isDisabled());
        HitTestListener<ClearLogRequest> listener = new HitTestListener<>();
        eventStudio().add(ClearLogRequest.class, listener);
        robot.clickOn(button);
        assertTrue(listener.isHit());
    }

    @Test
    public void closeIsInvisible() {
        app().runtimeState().activeTool(null);
        waitForFxEvents();
        FxAssert.verifyThat(i18n().tr("C_lose"), NodeMatchers.isInvisible());
    }

    @Test
    public void closeIsVisible() {
        app().runtimeState().activeTool(new DefaultPriorityTestTool());
        waitForFxEvents();
        FxAssert.verifyThat("C_lose", NodeMatchers.isVisible());
        HitTestListener<SetActiveContentItemRequest> listener = new HitTestListener<>();
        eventStudio().add(SetActiveContentItemRequest.class, listener);
        robot.clickOn(i18n().tr("C_lose"));
        assertTrue(listener.isHit());
    }
}