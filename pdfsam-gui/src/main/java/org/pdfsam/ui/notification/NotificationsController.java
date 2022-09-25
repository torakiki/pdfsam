/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
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
package org.pdfsam.ui.notification;

import static org.pdfsam.ui.commons.UrlButton.styledUrlButton;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.nio.file.AccessDeniedException;
import java.security.SecureRandom;

import javax.inject.Inject;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.news.NewImportantNewsEvent;
import org.pdfsam.update.NoUpdateAvailable;
import org.pdfsam.update.UpdateAvailableEvent;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;

/**
 * Component dealing with events that require a visual notification
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class NotificationsController {

    private static final int TIMES_BEFORE_ENTERPRISE_NOTICE = 5;

    private NotificationsContainer container;
    private UsageService service;
    private AppBrand appBrand;
    private UserContext userContext;
    private SecureRandom random = new SecureRandom();

    @Inject
    NotificationsController(NotificationsContainer container, UsageService service, AppBrand appBrand,
            UserContext userContext) {
        this.container = container;
        this.service = service;
        this.appBrand = appBrand;
        this.userContext = userContext;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddRequest(AddNotificationRequestEvent event) {
        container.addNotification(event.title, buildLabel(event.message, event.type));
    }

    private Label buildLabel(String message, NotificationType type) {
        Label textLabel = new Label(message);
        textLabel.getStyleClass().add("notification-text");
        if (type != null) {
            textLabel.getStyleClass().add(type.getStyleClass());
            textLabel.setGraphic(type.getGraphic());
        }
        return textLabel;
    }

    @EventListener
    public void onTaskFailed(TaskExecutionFailedEvent e) {
        if (e.getFailingCause() instanceof InvalidTaskParametersException) {
            container.addNotification(i18n().tr("Invalid parameters"), buildLabel(I18nContext.getInstance()
                            .i18n("Input parameters are invalid, open the application messages for details."),
                    NotificationType.ERROR));
        }
        Throwable root = ExceptionUtils.getRootCause(e.getFailingCause());
        if (root instanceof AccessDeniedException) {
            container.addNotification(i18n().tr("Access denied"), buildLabel(i18n().tr(
                    "Unable to access \"{0}\", please make sure you have write permissions or open the application messages for details.",
                    ((AccessDeniedException) root).getFile()), NotificationType.ERROR));
        }
    }

    @EventListener
    public void onTaskCompleted(@SuppressWarnings("unused") TaskExecutionCompletedEvent e) {
        long usages = service.getTotalUsage();
        if ((usages % TIMES_BEFORE_ENTERPRISE_NOTICE) == 0 && userContext.isDonationNotification()) {
            if ((random.nextInt() % 2) == 0) {
                addDonationNotification(usages);
            } else {
                addShareNotification(usages);
            }
        }
    }

    private void addDonationNotification(long usages) {
        VBox content = new VBox(3,
                buildLabel(i18n().tr("You performed {0} tasks with PDFsam, did it help?", Long.toString(usages)),
                        NotificationType.GO_PRO),
                styledUrlButton(i18n().tr("Give something back"), appBrand.property(BrandableProperty.DONATE_URL),
                        null));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(i18n().tr("PDFsam worked hard!"), content);
    }

    private void addShareNotification(long usages) {
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        VBox content = new VBox(3,
                buildLabel(i18n().tr("You performed {0} tasks with PDFsam, did it help?", Long.toString(usages)),
                        NotificationType.SHARE), new HBox(3, spacer,
                styledUrlButton(null, appBrand.property(BrandableProperty.FACEBOOK_SHARE_URL),
                        FontAwesomeIcon.FACEBOOK),
                styledUrlButton(i18n().tr("Spread the word!"), appBrand.property(BrandableProperty.TWEETER_SHARE_URL),
                        FontAwesomeIcon.TWITTER)));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(i18n().tr("PDFsam worked hard!"), content);
    }

    @EventListener
    public void onRemoveRequest(RemoveNotificationRequestEvent event) {
        container.removeNotification(event.notificationId);
    }

    @EventListener
    public void onUpdateAvailable(UpdateAvailableEvent event) {
        VBox content = new VBox(3, buildLabel(i18n().tr("PDFsam {0} is available for download", event.availableVersion),
                NotificationType.INFO),
                styledUrlButton(i18n().tr("Download"), appBrand.property(BrandableProperty.DOWNLOAD_URL), null));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(i18n().tr("New version available"), content);
    }

    @EventListener
    public void onNoUpdateAvailable(NoUpdateAvailable event) {
        VBox content = new VBox(3,
                buildLabel(i18n().tr("You are running the latest version of PDFsam Basic"), NotificationType.INFO));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addNotification(i18n().tr("No update"), content);
    }

    @EventListener
    public void onNewImportantNews(NewImportantNewsEvent event) {
        VBox content = new VBox(3, buildLabel(event.news.getContent(), null),
                styledUrlButton(i18n().tr("Open"), event.news.getLink(), FontAwesomeIcon.EXTERNAL_LINK));
        content.setAlignment(Pos.TOP_RIGHT);

        container.addStickyNotification(event.news.getTitle(), content);
    }
}
