/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19 gen 2016
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule("LogStage");

    private Footer victim;

    @Before
    public void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
        victim = new Footer(new RunButton());
    }

    @Test
    public void hideButtonsOnInit() {
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
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
        TaskExecutionRequestEvent event = mock(TaskExecutionRequestEvent.class);
        AbstractParameters params = mock(AbstractParameters.class);
        TaskOutput output = mock(FileTaskOutput.class);
        when(event.getParameters()).thenReturn(params);
        when(params.getOutput()).thenReturn(output);
        victim.onTaskExecutionRequest(event);
        assertFalse(victim.lookup(".footer-failed-button").isVisible());
        assertFalse(victim.lookup(".footer-open-button").isVisible());
        assertEquals(DefaultI18nContext.getInstance().i18n("Requested"),
                ((Labeled) victim.lookup(".status-label")).getText());
        assertEquals(0, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
        verify(output).accept(any());
    }
}
