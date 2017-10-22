/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19 gen 2016
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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.math.BigDecimal;
import java.util.Locale;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.TaskOutput;
import org.sejda.model.parameter.base.AbstractParameters;
import org.sejda.model.task.NotifiableTaskMetadata;

import javafx.scene.control.Labeled;
import javafx.scene.control.ProgressBar;

/**
 * @author Andrea Vacondio
 *
 */
public class FooterTest {
    private static final String MODULE_ID = "moduleId";
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    private Footer victim;

    @Before
    public void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
        OpenButton button = new OpenButton(MODULE_ID, ModuleInputOutputType.SINGLE_PDF);
        victim = new Footer(new RunButton(), button, MODULE_ID);
    }

    @Test
    public void hideOnInit() {
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertFalse(victim.lookup(".status-label").isVisible());
    }

    @Test
    public void onTaskCompleted() {
        TaskExecutionCompletedEvent event = mock(TaskExecutionCompletedEvent.class);
        victim.onTaskCompleted(event);
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertTrue(victim.lookup(".footer-open-button").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Completed"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertEquals(1, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onTaskFailed() {
        TaskExecutionFailedEvent event = mock(TaskExecutionFailedEvent.class);
        victim.onTaskFailed(event);
        assertTrue(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Failed"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertEquals(0, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onProgress() {
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        PercentageOfWorkDoneChangedEvent event = new PercentageOfWorkDoneChangedEvent(new BigDecimal(50), taskMetadata);
        victim.onProgress(event);
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Running {0}%", "50"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertEquals(0.5, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onProgressIndeterminate() {
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        PercentageOfWorkDoneChangedEvent event = new PercentageOfWorkDoneChangedEvent(
                PercentageOfWorkDoneChangedEvent.UNDETERMINED, taskMetadata);
        victim.onProgress(event);
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Running"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertTrue(((ProgressBar) victim.lookup(".pdfsam-footer-bar")).isIndeterminate());
    }

    @Test
    public void onTaskExecutionRequest() throws TaskOutputVisitException {
        AbstractParameters params = mock(AbstractParameters.class);
        TaskExecutionRequestEvent event = new TaskExecutionRequestEvent(MODULE_ID, params);
        TaskOutput output = mock(FileTaskOutput.class);
        when(params.getOutput()).thenReturn(output);
        eventStudio().broadcast(event);
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertTrue(victim.lookup(".status-label").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Requested"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertEquals(0, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
        verify(output).accept(any());
    }

    @Test
    public void onTaskExecutionRequestDifferentModule() {
        AbstractParameters params = mock(AbstractParameters.class);
        TaskExecutionRequestEvent event = new TaskExecutionRequestEvent("AnotherModule", params);
        TaskOutput output = mock(FileTaskOutput.class);
        when(params.getOutput()).thenReturn(output);
        eventStudio().broadcast(event);
        assertFalse(victim.lookup(".status-label").isVisible());
    }
}
