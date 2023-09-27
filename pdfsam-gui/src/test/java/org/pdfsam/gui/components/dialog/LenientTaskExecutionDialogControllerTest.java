/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09 feb 2017
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.exception.TaskNonLenientExecutionException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.AbstractParameters;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.io.IOException;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
public class LenientTaskExecutionDialogControllerTest {

    private Button button;
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        new LenientTaskExecutionDialogController(() -> new LenientExecutionConfirmationDialog(stage));
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void negativeWrongFailureCauseTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        var request = new TaskExecutionRequest("id", params);
        eventStudio().broadcast(request);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(new IOException(), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        robot.clickOn("show");
        assertFalse(robot.robotContext().getWindowFinder().listWindows().size() > 1);
    }

    @Test
    public void negativeNoLatestTest() {
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        robot.clickOn("show");
        assertFalse(robot.robotContext().getWindowFinder().listWindows().size() > 1);
    }

    @Test
    public void negativeNoTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        var request = new TaskExecutionRequest("id", params);
        eventStudio().broadcast(request);
        Listener<TaskExecutionRequest> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionRequest.class, listener);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        robot.clickOn("show");
        assertTrue(robot.robotContext().getWindowFinder().listWindows().size() > 1);
        robot.clickOn(i18n().tr("No"));
        verify(params, never()).setLenient(anyBoolean());
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void positiveTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        var request = new TaskExecutionRequest("id", params);
        eventStudio().broadcast(request);
        Listener<TaskExecutionRequest> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionRequest.class, listener);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        robot.clickOn("show");
        assertTrue(robot.robotContext().getWindowFinder().listWindows().size() > 1);
        robot.clickOn(i18n().tr("Yes"));
        verify(params).setLenient(true);
        verify(listener).onEvent(request);
    }

}
