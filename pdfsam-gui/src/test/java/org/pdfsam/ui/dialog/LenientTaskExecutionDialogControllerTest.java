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
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.eventstudio.Listener;
import org.sejda.injector.Components;
import org.sejda.injector.Injector;
import org.sejda.injector.Provides;
import org.sejda.model.exception.TaskNonLenientExecutionException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.AbstractParameters;

import javafx.scene.Parent;
import javafx.scene.control.Button;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class LenientTaskExecutionDialogControllerTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        Injector.start(new Config());
        Button button = new Button("show");
        return button;
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
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(failure));
        click("show");
        assertFalse(getWindows().size() > 1);
    }

    @Test
    public void negativeNoLatestTest() {
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(failure));
        click("show");
        assertFalse(getWindows().size() > 1);
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
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(failure));
        click("show");
        assertTrue(getWindows().size() > 1);
        click(DefaultI18nContext.getInstance().i18n("No"));
        verify(params, never()).setLenient(anyBoolean());
        verify(listener, never()).onEvent(any());
    }

    @Test
    public void positiveTest() {
        AbstractParameters params = mock(AbstractParameters.class);
        TaskExecutionRequestEvent request = new TaskExecutionRequestEvent("id", params);
        eventStudio().broadcast(request);
        Listener<TaskExecutionRequestEvent> listener = mock(Listener.class);
        eventStudio().add(TaskExecutionRequestEvent.class, listener);
        TaskExecutionFailedEvent failure = new TaskExecutionFailedEvent(
                new TaskNonLenientExecutionException(new IOException()), null);
        Button button = find("show");
        button.setOnAction(a -> eventStudio().broadcast(failure));
        click("show");
        assertTrue(getWindows().size() > 1);
        click(DefaultI18nContext.getInstance().i18n("Yes"));
        verify(params).setLenient(true);
        verify(listener).onEvent(request);
    }

}
