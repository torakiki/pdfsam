/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
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
package org.pdfsam.ui.notification;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import javax.inject.Inject;

import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.update.UpdateAvailableEvent;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class NotificationsControllerTest {

    @ClassRule
    public static ClearEventStudioRule STUDIO_RULE = new ClearEventStudioRule();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Inject
    private ApplicationContext applicationContext;

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public NotificationsController victim() {
            return new NotificationsController();
        }

        @Bean
        public NotificationsContainer container() {
            return mock(NotificationsContainer.class);
        }
    }

    @Test
    public void onAddRequest() {
        AddNotificationRequestEvent event = new AddNotificationRequestEvent(NotificationType.INFO, "msg", "title");
        applicationContext.getBean(NotificationsController.class).onAddRequest(event);
        verify(applicationContext.getBean(NotificationsContainer.class)).addNotification(eq("title"), any());
    }

    @Test
    public void onRemoveRequest() {
        RemoveNotificationRequestEvent event = new RemoveNotificationRequestEvent("id");
        applicationContext.getBean(NotificationsController.class).onRemoveRequest(event);
        verify(applicationContext.getBean(NotificationsContainer.class)).removeNotification("id");
    }

    @Test
    public void onUpdateAvailable() {
        UpdateAvailableEvent event = new UpdateAvailableEvent("new version");
        applicationContext.getBean(NotificationsController.class).onUpdateAvailable(event);
        verify(applicationContext.getBean(NotificationsContainer.class)).addStickyNotification(anyString(), any());
    }

    @Test
    public void onTaskFailed() {
        TaskExecutionFailedEvent event = new TaskExecutionFailedEvent(new Exception("some exception"), null);
        applicationContext.getBean(NotificationsController.class).onTaskFailed(event);
        verify(applicationContext.getBean(NotificationsContainer.class), never()).addNotification(anyString(), any());
    }

    @Test
    public void onInvalidParameters() {
        TaskExecutionFailedEvent event = new TaskExecutionFailedEvent(new InvalidTaskParametersException(), null);
        applicationContext.getBean(NotificationsController.class).onTaskFailed(event);
        verify(applicationContext.getBean(NotificationsContainer.class)).addNotification(anyString(), any());
    }
}
