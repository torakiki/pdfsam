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
package org.pdfsam.ui.dialog;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.exception.TaskNonLenientExecutionException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.AbstractParameters;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class LenientTaskExecutionDialogControllerTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Button button;

    @BeforeClass
    public static void setUp() {
        ((DefaultI18nContext) DefaultI18nContext.getInstance()).refresh(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        Injector.start(new Config());
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Components({ LenientTaskExecutionDialogController.class })
    static class Config {

        @Provides
        StylesConfig style() {
            return mock(StylesConfig.class);
        }

    }

    @Test
    public void negativeWrongFailureCauseTest() {
        TaskExecutionRequestEvent request = mock(TaskExecutionRequestEvent.class);
        eventStudio().broadcast(request);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(new IOException(), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        clickOn("show");
        assertFalse(robotContext().getWindowFinder().listWindows().size() > 1);
    }

    @Test
    public void negativeNoLatestTest() {
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        clickOn("show");
        assertFalse(robotContext().getWindowFinder().listWindows().size() > 1);
    }

    @Test
    public void negativeNoTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        TaskExecutionRequestEvent request = new TaskExecutionRequestEvent("id", params);
        eventStudio().broadcast(request);
        Listener<TaskExecutionRequestEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionRequestEvent.class, listener);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        clickOn("show");
        assertTrue(robotContext().getWindowFinder().listWindows().size() > 1);
        clickOn(DefaultI18nContext.getInstance().i18n("No"));
        verify(params, never()).setLenient(anyBoolean());
        verify(listener, never()).onEvent(any());
    }

    @Test
    @Category(NoHeadless.class)
    public void positiveTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        TaskExecutionRequestEvent request = new TaskExecutionRequestEvent("id", params);
        eventStudio().broadcast(request);
        Listener<TaskExecutionRequestEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionRequestEvent.class, listener);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        button.setOnAction(a -> eventStudio().broadcast(failure));
        clickOn("show");
        assertTrue(robotContext().getWindowFinder().listWindows().size() > 1);
        clickOn(DefaultI18nContext.getInstance().i18n("Yes"));
        verify(params).setLenient(true);
        verify(listener).onEvent(request);
    }

}
