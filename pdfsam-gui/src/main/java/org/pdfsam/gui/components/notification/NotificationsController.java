/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import jakarta.inject.Inject;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.gui.components.content.log.LogContentItem;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.news.NewImportantNewsEvent;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.update.NoUpdateAvailable;
import org.pdfsam.model.update.UpdateAvailableEvent;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.ui.components.commons.UrlButton;
import org.pdfsam.ui.components.notification.AddNotificationRequest;
import org.pdfsam.ui.components.notification.NotificationType;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.exception.InvalidTaskParametersException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;

import java.nio.file.AccessDeniedException;
import java.security.SecureRandom;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component dealing with events that require a visual notification
 *
 * @author Andrea Vacondio
 */
@Auto
public class NotificationsController {

    private static final int TIMES_BEFORE_DONATION_NOTICE = 5;

    private final NotificationsContainer container;
    private final UsageService service;
    private final AppBrand appBrand;
    private final SecureRandom random = new SecureRandom();

    @Inject
    NotificationsController(NotificationsContainer container, UsageService service, AppBrand appBrand) {
        this.container = container;
        this.service = service;
        this.appBrand = appBrand;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddRequest(AddNotificationRequest event) {
        container.addNotification(event.title(), buildLabel(event.message(), event.type()));
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

            var showErrors = new Button(i18n().tr("Show errors"));
            showErrors.setOnAction(
                    event -> eventStudio().broadcast(new SetActiveContentItemRequest(LogContentItem.ID)));
            showErrors.getStyleClass().addAll(Style.BUTTON.css());

            var content = new VBox(buildLabel(i18n().tr("Input parameters are invalid"), NotificationType.ERROR),
                    showErrors);
            content.getStyleClass().add("notification-container");

            container.addNotification(i18n().tr("Invalid parameters"), content);
        }
        Throwable root = ExceptionUtils.getRootCause(e.getFailingCause());
        if (root instanceof AccessDeniedException) {
            container.addNotification(i18n().tr("Access denied"), buildLabel(i18n().tr(
                    "Unable to access \"{0}\", please make sure you have write permissions or open the application messages for details.",
                    ((AccessDeniedException) root).getFile()), NotificationType.ERROR));
        }
    }

    @EventListener
    public void onTaskCompleted(TaskExecutionCompletedEvent e) {
        long usages = service.getTotalUsages();
        if ((usages % TIMES_BEFORE_DONATION_NOTICE) == 0 && app().persistentSettings()
                .get(BooleanPersistentProperty.DONATION_NOTIFICATION)) {
            if ((random.nextInt() % 2) == 0) {
                addDonationNotification(usages);
            } else {
                addShareNotification(usages);
            }
        }
    }

    private void addDonationNotification(long usages) {
        var content = new VBox(
                buildLabel(i18n().tr("You performed {0} tasks with PDFsam, did it help?", Long.toString(usages)),
                        NotificationType.SUPPORT), UrlButton.styledUrlButton(i18n().tr("Give something back"),
                appBrand.property(BrandableProperty.DONATE_URL), null));
        content.getStyleClass().add("notification-container");

        container.addStickyNotification(i18n().tr("PDFsam worked hard!"), content);
    }

    private void addShareNotification(long usages) {
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        var content = new VBox(
                buildLabel(i18n().tr("You performed {0} tasks with PDFsam, did it help?", Long.toString(usages)),
                        NotificationType.SHARE), new HBox(3, spacer,
                UrlButton.styledUrlButton(null, appBrand.property(BrandableProperty.FACEBOOK_SHARE_URL),
                        UniconsLine.FACEBOOK), UrlButton.styledUrlButton(i18n().tr("Spread the word!"),
                appBrand.property(BrandableProperty.TWEETER_SHARE_URL), UniconsLine.TWITTER)));
        content.getStyleClass().add("notification-container");

        container.addStickyNotification(i18n().tr("PDFsam worked hard!"), content);
    }

    @EventListener
    public void onRemoveRequest(RemoveNotificationRequest event) {
        container.removeNotification(event.id());
    }

    @EventListener
    public void onUpdateAvailable(UpdateAvailableEvent event) {
        var content = new VBox(buildLabel(i18n().tr("PDFsam {0} is available for download", event.availableVersion()),
                NotificationType.INFO),
                UrlButton.styledUrlButton(i18n().tr("Download"), appBrand.property(BrandableProperty.DOWNLOAD_URL),
                        null));
        content.getStyleClass().add("notification-container");

        container.addStickyNotification(i18n().tr("New version available"), content);
    }

    @EventListener
    public void onNoUpdateAvailable(NoUpdateAvailable event) {
        var content = new VBox(
                buildLabel(i18n().tr("You are running the latest version of PDFsam Basic"), NotificationType.INFO));
        content.getStyleClass().add("notification-container");

        container.addNotification(i18n().tr("No update"), content);
    }

    @EventListener
    public void onNewImportantNews(NewImportantNewsEvent event) {
        var content = new VBox(buildLabel(event.news().content(), null),
                UrlButton.styledUrlButton(i18n().tr("Open"), event.news().link(), UniconsLine.EXTERNAL_LINK_ALT));
        content.getStyleClass().add("notification-container");

        container.addStickyNotification(event.news().title(), content);
    }
}
