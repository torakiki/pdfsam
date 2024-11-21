/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
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
package org.pdfsam.gui.components.notification;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.model.news.NewImportantNewsEvent;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.model.update.NoUpdateAvailable;
import org.pdfsam.model.update.UpdateAvailableEvent;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.ui.components.notification.AddNotificationRequest;
import org.pdfsam.ui.components.notification.NotificationType;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.exception.TaskIOException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import java.nio.file.AccessDeniedException;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class NotificationsControllerTest {

    private UsageService service;
    private NotificationsContainer container;
    private NotificationsController victim;

    @BeforeEach
    public void setUp() {
        service = mock(UsageService.class);
        container = mock(NotificationsContainer.class);
        AppBrand appBrand = mock(AppBrand.class);
        when(appBrand.property(BrandableProperty.DOWNLOAD_URL)).thenReturn("http://www.pdfsam.org");
        when(appBrand.property(BrandableProperty.DONATE_URL)).thenReturn("http://www.pdfsam.org");
        when(appBrand.property(BrandableProperty.BLUESKY_SHARE_URL)).thenReturn("http://www.pdfsam.org");
        when(appBrand.property(BrandableProperty.FACEBOOK_SHARE_URL)).thenReturn("http://www.pdfsam.org");
        victim = new NotificationsController(container, service, appBrand);
        app().persistentSettings().set(BooleanPersistentProperty.DONATION_NOTIFICATION, true);
    }

    @Test
    public void onAddRequest() {
        var event = new AddNotificationRequest(NotificationType.INFO, "msg", "title");
        victim.onAddRequest(event);
        verify(container).addNotification(eq("title"), any());
    }

    @Test
    public void onRemoveRequest() {
        var event = new RemoveNotificationRequest("id");
        victim.onRemoveRequest(event);
        verify(container).removeNotification("id");
    }

    @Test
    public void onUpdateAvailable() {
        var event = new UpdateAvailableEvent("new version");
        victim.onUpdateAvailable(event);
        verify(container).addStickyNotification(anyString(), any());
    }

    @Test
    public void onNoUpdateAvailable() {
        var event = new NoUpdateAvailable();
        victim.onNoUpdateAvailable(event);
        verify(container).addNotification(anyString(), any());
    }

    @Test
    public void onTaskFailed() {
        TaskExecutionFailedEvent event = new TaskExecutionFailedEvent(new Exception("some exception"), null);
        victim.onTaskFailed(event);
        verify(container, never()).addNotification(anyString(), any());
    }

    @Test
    public void onInvalidParameters() {
        TaskExecutionFailedEvent event = new TaskExecutionFailedEvent(
                new InvalidTaskParametersException("", Collections.emptyList()), null);
        victim.onTaskFailed(event);
        verify(container).addNotification(anyString(), any());
    }

    @Test
    public void onAccessDenied() {
        TaskExecutionFailedEvent event = new TaskExecutionFailedEvent(
                new TaskIOException(new AccessDeniedException("the file")), null);
        victim.onTaskFailed(event);
        verify(container).addNotification(anyString(), any());
    }

    @Test
    public void onTaskCompleteAndNoProDisplay() {
        when(service.getTotalUsages()).thenReturn(1L);
        victim.onTaskCompleted(new TaskExecutionCompletedEvent(1, null));
        when(service.getTotalUsages()).thenReturn(6L);
        victim.onTaskCompleted(new TaskExecutionCompletedEvent(1, null));
        verify(container, never()).addStickyNotification(anyString(), any());
    }

    @Test
    public void onTaskCompleteAndProDisplay() {
        when(service.getTotalUsages()).thenReturn(5L);
        victim.onTaskCompleted(new TaskExecutionCompletedEvent(1, null));
        verify(container).addStickyNotification(anyString(), any());
    }

    @Test
    public void onTaskCompleteDontDisplaySettingIsOn() {
        app().persistentSettings().set(BooleanPersistentProperty.DONATION_NOTIFICATION, false);
        when(service.getTotalUsages()).thenReturn(5L);
        victim.onTaskCompleted(new TaskExecutionCompletedEvent(1, null));
        verify(container, never()).addStickyNotification(anyString(), any());
    }

    @Test
    public void onNewImportantNews() {
        NewsData data = new NewsData(5, "title", "content", "20221010", "link", true);
        victim.onNewImportantNews(new NewImportantNewsEvent(data));
        verify(container).addStickyNotification(eq("title"), any());
    }
}
