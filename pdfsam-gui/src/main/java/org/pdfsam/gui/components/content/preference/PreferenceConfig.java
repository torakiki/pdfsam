/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/lug/2014
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
package org.pdfsam.gui.components.content.preference;

import jakarta.inject.Named;
import javafx.util.Subscription;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.gui.components.content.log.MaxLogRowsChangedEvent;
import org.pdfsam.gui.theme.Themes;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.DefaultPdfVersionComboItem;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.pdf.PdfVersion;

import java.util.Arrays;
import java.util.Comparator;
import java.util.stream.IntStream;

import static java.util.Comparator.comparing;
import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.context.BooleanPersistentProperty.CHECK_FOR_NEWS;
import static org.pdfsam.core.context.BooleanPersistentProperty.CHECK_UPDATES;
import static org.pdfsam.core.context.BooleanPersistentProperty.CLEAR_CONFIRMATION;
import static org.pdfsam.core.context.BooleanPersistentProperty.DISCARD_BOOKMARKS;
import static org.pdfsam.core.context.BooleanPersistentProperty.DONATION_NOTIFICATION;
import static org.pdfsam.core.context.BooleanPersistentProperty.OVERWRITE_OUTPUT;
import static org.pdfsam.core.context.BooleanPersistentProperty.PDF_COMPRESSION_ENABLED;
import static org.pdfsam.core.context.BooleanPersistentProperty.PLAY_SOUNDS;
import static org.pdfsam.core.context.BooleanPersistentProperty.PREMIUM_MODULES;
import static org.pdfsam.core.context.BooleanPersistentProperty.SAVE_PWD_IN_WORKSPACE;
import static org.pdfsam.core.context.BooleanPersistentProperty.SAVE_WORKSPACE_ON_EXIT;
import static org.pdfsam.core.context.BooleanPersistentProperty.SMART_OUTPUT;
import static org.pdfsam.core.context.IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER;
import static org.pdfsam.core.context.StringPersistentProperty.FONT_SIZE;
import static org.pdfsam.core.context.StringPersistentProperty.PDF_VERSION;
import static org.pdfsam.core.context.StringPersistentProperty.STARTUP_MODULE;
import static org.pdfsam.core.context.StringPersistentProperty.THEME;
import static org.pdfsam.core.context.StringPersistentProperty.WORKING_PATH;
import static org.pdfsam.core.context.StringPersistentProperty.WORKSPACE_PATH;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.model.ui.ComboItem.keyWithEmptyValue;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Configuration for the PDFsam preferences components
 *
 * @author Andrea Vacondio
 */
public class PreferenceConfig {

    @Provides
    @Named("localeCombo")
    public PreferenceComboBox<ComboItem<String>> localeCombo() {
        return new PreferenceComboBox<>(StringPersistentProperty.LOCALE);
    }

    @Provides
    @Named("startupToolCombo")
    public PreferenceComboBox<ComboItem<String>> startupToolCombo() {
        PreferenceComboBox<ComboItem<String>> startupToolCombo = new PreferenceComboBox<>(STARTUP_MODULE);
        startupToolCombo.setId("startupModuleCombo");
        startupToolCombo.getItems().add(new ComboItem<>("", i18n().tr("Home")));
        app().runtimeState().tools().values().stream().map(tool -> new ComboItem<>(tool.id(), tool.descriptor().name()))
                .sorted(comparing(ComboItem::description)).forEach(startupToolCombo.getItems()::add);
        startupToolCombo.setValue(keyWithEmptyValue(app().persistentSettings().get(STARTUP_MODULE).orElse("")));
        return startupToolCombo;
    }

    @Provides
    @Named("themeCombo")
    public PreferenceComboBox<ComboItem<String>> themeCombo() {
        PreferenceComboBox<ComboItem<String>> themeCombo = new PreferenceComboBox<>(THEME);
        themeCombo.setId("themeCombo");
        Themes.themes().entrySet().stream().sorted(Comparator.comparing(e -> e.getValue().name()))
                .map(entry -> new ComboItem<>(entry.getKey(), entry.getValue().name()))
                .forEach(themeCombo.getItems()::add);
        final Subscription[] subscription = new Subscription[1];
        subscription[0] = app().runtimeState().theme().subscribe(t -> {
            if (nonNull(t)) {
                themeCombo.setValue(new ComboItem<>(t.id(), t.name()));
                themeCombo.valueProperty().addListener(
                        (observable, oldVal, newVal) -> ofNullable(Themes.get(newVal.key())).ifPresent(
                                theme -> app().runtimeState().theme(theme)));
                ofNullable(subscription[0]).ifPresent(Subscription::unsubscribe);
            }
        });
        return themeCombo;
    }

    @Provides
    @Named("fontSizeCombo")
    public PreferenceComboBox<ComboItem<String>> fontSizeCombo() {
        PreferenceComboBox<ComboItem<String>> fontSizeCombo = new PreferenceComboBox<>(FONT_SIZE);
        fontSizeCombo.setId("fontSizeCombo");
        fontSizeCombo.getItems().add(new ComboItem<>("", i18n().tr("System default")));
        IntStream.range(9, 37).forEach(i -> fontSizeCombo.getItems().add(new ComboItem<>(i + "px", i + "px")));
        fontSizeCombo.setValue(keyWithEmptyValue(app().persistentSettings().get(FONT_SIZE).orElse("")));
        return fontSizeCombo;
    }

    @Provides
    @Named("pdfVersionCombo")
    public PreferenceComboBox<ComboItem<PdfVersion>> pdfVersionCombo() {
        PreferenceComboBox<ComboItem<PdfVersion>> pdfVersionCombo = new PreferenceComboBox<>(PDF_VERSION);
        pdfVersionCombo.setId("pdfVersionCombo");
        pdfVersionCombo.getItems().addAll(Arrays.stream(PdfVersion.values())
                .filter(v -> v.getVersion() > PdfVersion.VERSION_1_2.getVersion()).map(DefaultPdfVersionComboItem::new)
                .toList());
        //select if present in the settings
        app().persistentSettings().get(StringPersistentProperty.PDF_VERSION).map(PdfVersion::valueOf)
                .flatMap(v -> pdfVersionCombo.getItems().stream().filter(i -> i.key() == v).findFirst())
                .ifPresent(i -> pdfVersionCombo.getSelectionModel().select(i));
        return pdfVersionCombo;
    }

    @Provides
    @Named("checkForUpdates")
    public PreferenceCheckBox checkForUpdates() {
        var checkForUpdates = new PreferenceCheckBox(CHECK_UPDATES, i18n().tr("Check for updates at startup"),
                app().persistentSettings().get(CHECK_UPDATES));
        checkForUpdates.setId("checkForUpdates");
        checkForUpdates.setGraphic(helpIcon(
                i18n().tr("Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().addAll(Style.WITH_HELP.css());
        checkForUpdates.getStyleClass().addAll(Style.VITEM.css());
        return checkForUpdates;
    }

    @Provides
    @Named("checkForNews")
    public PreferenceCheckBox checkForNews() {
        var checkForNews = new PreferenceCheckBox(CHECK_FOR_NEWS, i18n().tr("Check for news at startup"),
                app().persistentSettings().get(CHECK_FOR_NEWS));
        checkForNews.setId("checkForNews");
        checkForNews.setGraphic(helpIcon(i18n().tr(
                "Set whether the application should check for news availability on startup (restart needed)")));
        checkForNews.getStyleClass().addAll(Style.WITH_HELP.css());
        checkForNews.getStyleClass().addAll(Style.VITEM.css());
        return checkForNews;
    }

    @Provides
    @Named("compressionEnabled")
    public PreferenceCheckBox compressionEnabled() {
        var compressionEnabled = new PreferenceCheckBox(PDF_COMPRESSION_ENABLED, i18n().tr("Enabled PDF compression"),
                app().persistentSettings().get(PDF_COMPRESSION_ENABLED));
        compressionEnabled.setId("compressionEnabled");
        compressionEnabled.setGraphic(
                helpIcon(i18n().tr("Set whether \"Compress output file\" should be enabled by default")));
        compressionEnabled.getStyleClass().addAll(Style.WITH_HELP.css());
        compressionEnabled.getStyleClass().addAll(Style.VITEM.css());
        return compressionEnabled;
    }

    @Provides
    @Named("discardBookmarks")
    public PreferenceCheckBox discardBookmarks() {
        var discardBookmarks = new PreferenceCheckBox(DISCARD_BOOKMARKS, i18n().tr("Discard bookmarks"),
                app().persistentSettings().get(DISCARD_BOOKMARKS));
        discardBookmarks.setId("discardBookmarks");
        discardBookmarks.setGraphic(
                helpIcon(i18n().tr("Set whether \"Discard bookmarks\" should be enabled by default")));
        discardBookmarks.getStyleClass().addAll(Style.WITH_HELP.css());
        discardBookmarks.getStyleClass().addAll(Style.VITEM.css());
        return discardBookmarks;
    }

    @Provides
    @Named("prefixField")
    public PreferencePrefixField prefixField() {
        return new PreferencePrefixField();
    }

    @Provides
    @Named("overwriteOutput")
    public PreferenceCheckBox overwriteOutput() {
        var overwriteOutput = new PreferenceCheckBox(OVERWRITE_OUTPUT, i18n().tr("Overwrite files"),
                app().persistentSettings().get(OVERWRITE_OUTPUT));
        overwriteOutput.setId("overwriteOutput");
        overwriteOutput.setGraphic(
                helpIcon(i18n().tr("Set whether \"Overwrite if already exists\" should be enabled by default")));
        overwriteOutput.getStyleClass().addAll(Style.WITH_HELP.css());
        overwriteOutput.getStyleClass().addAll(Style.VITEM.css());
        return overwriteOutput;
    }

    @Provides
    @Named("playSounds")
    public PreferenceCheckBox playSounds() {
        var playSounds = new PreferenceCheckBox(PLAY_SOUNDS, i18n().tr("Play alert sounds"),
                app().persistentSettings().get(PLAY_SOUNDS));
        playSounds.setId("playSounds");
        playSounds.setGraphic(helpIcon(i18n().tr("Turn on or off alert sounds")));
        playSounds.getStyleClass().addAll(Style.WITH_HELP.css());
        playSounds.getStyleClass().addAll(Style.VITEM.css());
        return playSounds;
    }

    @Provides
    @Named("savePwdInWorkspace")
    public PreferenceCheckBox savePwdInWorkspace() {
        var savePwdInWorkspace = new PreferenceCheckBox(SAVE_PWD_IN_WORKSPACE,
                i18n().tr("Store passwords when saving a workspace file"),
                app().persistentSettings().get(SAVE_PWD_IN_WORKSPACE));
        savePwdInWorkspace.setId("savePwdInWorkspace");
        savePwdInWorkspace.setGraphic(helpIcon(i18n().tr(
                "If an encrypted PDF document has been opened with a password, save the password in the workspace file")));
        savePwdInWorkspace.getStyleClass().addAll(Style.WITH_HELP.css());
        savePwdInWorkspace.getStyleClass().addAll(Style.VITEM.css());
        return savePwdInWorkspace;
    }

    @Provides
    @Named("donationNotification")
    public PreferenceCheckBox donationNotification() {
        var donationNotification = new PreferenceCheckBox(DONATION_NOTIFICATION, i18n().tr("Show donation window"),
                app().persistentSettings().get(DONATION_NOTIFICATION));
        donationNotification.setId("donationNotification");
        donationNotification.setGraphic(helpIcon(i18n().tr(
                "Turn on or off the notification appearing once in a while and asking the user to support PDFsam with a donation")));
        donationNotification.getStyleClass().addAll(Style.WITH_HELP.css());
        donationNotification.getStyleClass().addAll(Style.VITEM.css());
        return donationNotification;
    }

    @Provides
    @Named("fetchPremiumModules")
    public PreferenceCheckBox fetchPremiumModules() {
        var fetchPremiumModules = new PreferenceCheckBox(PREMIUM_MODULES, i18n().tr("Show premium features"),
                app().persistentSettings().get(PREMIUM_MODULES));
        fetchPremiumModules.setId("fetchPremiumModules");
        fetchPremiumModules.setGraphic(helpIcon(i18n().tr(
                "Set whether the application should fetch and show premium features description in the modules dashboard")));
        fetchPremiumModules.getStyleClass().addAll(Style.WITH_HELP.css());
        fetchPremiumModules.getStyleClass().addAll(Style.VITEM.css());
        return fetchPremiumModules;
    }

    @Provides
    @Named("clearConfirmation")
    public PreferenceCheckBox clearConfirmation() {
        var clearConfirmation = new PreferenceCheckBox(CLEAR_CONFIRMATION,
                i18n().tr("Ask for a confirmation when clearing the selection table"),
                app().persistentSettings().get(CLEAR_CONFIRMATION));
        clearConfirmation.setId("clearConfirmation");
        clearConfirmation.setGraphic(helpIcon(i18n().tr(
                "Set whether the application should ask for a confirmation when clearing the selection table")));
        clearConfirmation.getStyleClass().addAll(Style.WITH_HELP.css());
        clearConfirmation.getStyleClass().addAll(Style.VITEM.css());
        return clearConfirmation;
    }

    @Provides
    @Named("smartRadio")
    public PreferenceRadioButton smartRadio() {
        var smartRadio = new PreferenceRadioButton(SMART_OUTPUT,
                i18n().tr("Use the selected PDF document directory as output directory"),
                app().persistentSettings().get(SMART_OUTPUT));
        smartRadio.setId("smartRadio");
        return smartRadio;
    }

    @Provides
    @Named("workingDirectory")
    public PreferenceBrowsableDirectoryField workingDirectory() {
        var workingDirectory = new PreferenceBrowsableDirectoryField(WORKING_PATH);
        workingDirectory.getTextField().setText(app().persistentSettings().get(WORKING_PATH).orElse(""));
        workingDirectory.setId("workingDirectory");
        return workingDirectory;
    }

    @Provides
    @Named("workspace")
    public PreferenceBrowsableFileField workspace() {
        var workspace = new PreferenceBrowsableFileField(WORKSPACE_PATH, FileType.JSON, OpenType.OPEN);
        workspace.getTextField().setText(app().persistentSettings().get(WORKSPACE_PATH).orElse(""));
        workspace.setId("workspace");
        return workspace;
    }

    @Provides
    @Named("saveWorkspaceOnExit")
    public PreferenceCheckBox saveWorkspaceOnExit() {
        var saveWorkspaceOnExit = new PreferenceCheckBox(SAVE_WORKSPACE_ON_EXIT,
                i18n().tr("Save default workspace on exit"), app().persistentSettings().get(SAVE_WORKSPACE_ON_EXIT));
        saveWorkspaceOnExit.setId("saveWorkspaceOnExit");
        saveWorkspaceOnExit.setGraphic(
                helpIcon(i18n().tr("If a default workspace is set, save it on application exit")));
        saveWorkspaceOnExit.getStyleClass().addAll(Style.WITH_HELP.css());
        saveWorkspaceOnExit.getStyleClass().add("spaced-vitem");
        return saveWorkspaceOnExit;
    }

    @Provides
    @Named("logViewRowsNumber")
    public PreferenceIntTextField logViewRowsNumber() {
        var logRowsNumber = new PreferenceIntTextField(LOGVIEW_ROWS_NUMBER, Validators.positiveInteger());
        logRowsNumber.setText(Integer.toString(app().persistentSettings().get(LOGVIEW_ROWS_NUMBER)));
        logRowsNumber.setErrorMessage(i18n().tr("Maximum number of rows mast be a positive number"));
        logRowsNumber.setId("logViewRowsNumber");
        logRowsNumber.validProperty().addListener((o, oldVal, newVal) -> {
            if (newVal == FXValidationSupport.ValidationState.VALID) {
                eventStudio().broadcast(new MaxLogRowsChangedEvent());
            }
        });
        return logRowsNumber;
    }

}
