/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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

import java.math.BigDecimal;

import javafx.scene.control.ProgressBar;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.TaskOutput;
import org.sejda.model.parameter.base.TaskParameters;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class ModuleFooterPaneTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule("LogStage");

    @Autowired
    private ApplicationContext applicationContext;

    @Configuration
    static class TestConfig {
        @Bean
        @Lazy
        @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
        public ModuleFooterPane victim() {
            return new ModuleFooterPane();
        }
    }

    @Test
    public void hideButtonsOnInit() {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        victim.lookupAll(".pdfsam-footer-button").forEach(b -> assertFalse(b.isVisible()));
    }

    @Test
    public void onTaskCompleted() {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        TaskExecutionCompletedEvent event = mock(TaskExecutionCompletedEvent.class);
        victim.onTaskCompleted(event);
        assertFalse(victim.lookup(".pdfsam-footer-failed-button").isVisible());
        assertTrue(victim.lookup(".pdfsam-footer-open-button").isVisible());
        assertEquals(1, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onTaskFailed() {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        TaskExecutionFailedEvent event = mock(TaskExecutionFailedEvent.class);
        victim.onTaskFailed(event);
        assertTrue(victim.lookup(".pdfsam-footer-failed-button").isVisible());
        assertFalse(victim.lookup(".pdfsam-footer-open-button").isVisible());
        assertEquals(1, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onProgress() {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        PercentageOfWorkDoneChangedEvent event = new PercentageOfWorkDoneChangedEvent(new BigDecimal(50), taskMetadata);
        victim.onProgress(event);
        assertFalse(victim.lookup(".pdfsam-footer-failed-button").isVisible());
        assertFalse(victim.lookup(".pdfsam-footer-open-button").isVisible());
        assertEquals(0.5, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
    }

    @Test
    public void onProgressIndeterminate() {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        PercentageOfWorkDoneChangedEvent event = new PercentageOfWorkDoneChangedEvent(
                PercentageOfWorkDoneChangedEvent.UNDETERMINED, taskMetadata);
        victim.onProgress(event);
        assertFalse(victim.lookup(".pdfsam-footer-failed-button").isVisible());
        assertFalse(victim.lookup(".pdfsam-footer-open-button").isVisible());
        assertTrue(((ProgressBar) victim.lookup(".pdfsam-footer-bar")).isIndeterminate());
    }

    @Test
    public void onTaskExecutionRequest() throws TaskOutputVisitException {
        ModuleFooterPane victim = applicationContext.getBean(ModuleFooterPane.class);
        TaskExecutionRequestEvent event = mock(TaskExecutionRequestEvent.class);
        TaskParameters params = mock(TaskParameters.class);
        TaskOutput output = mock(FileTaskOutput.class);
        when(event.getParameters()).thenReturn(params);
        when(params.getOutput()).thenReturn(output);
        victim.onTaskExecutionRequest(event);
        assertFalse(victim.lookup(".pdfsam-footer-failed-button").isVisible());
        assertFalse(victim.lookup(".pdfsam-footer-open-button").isVisible());
        assertEquals(0, ((ProgressBar) victim.lookup(".pdfsam-footer-bar")).getProgress(), 0.01);
        verify(output).accept(any());
    }
}
