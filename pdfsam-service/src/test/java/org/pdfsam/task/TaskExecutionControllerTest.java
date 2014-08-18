/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam.task;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

import javax.inject.Inject;

import org.junit.AfterClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.module.UsageService;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.model.parameter.base.TaskParameters;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class TaskExecutionControllerTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Inject
    private ExecutionService executionService;
    @Inject
    private UsageService usageService;
    @Inject
    private TaskExecutionController victim;

    @AfterClass
    public static void tearDown() {
        GlobalNotificationContext.getContext().clearListeners();
    }

    @Configuration
    static class Config {
        @Bean
        public UsageService usageService() {
            return mock(UsageService.class);
        }

        @Bean
        public ExecutionService executionService() {
            return mock(ExecutionService.class);
        }

        @Bean
        public TaskExecutionController controller() {
            return new TaskExecutionController();
        }
    }

    @Test
    public void request() {
        String moduleId = "module";
        TaskParameters params = mock(TaskParameters.class);
        victim.request(new TaskExecutionRequestEvent(moduleId, params));
        verify(usageService).incrementUsageFor(moduleId);
        verify(executionService, timeout(1000).times(1)).submit(moduleId, params);
    }

}
