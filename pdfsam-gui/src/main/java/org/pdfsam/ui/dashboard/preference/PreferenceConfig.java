/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/lug/2014
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
package org.pdfsam.ui.dashboard.preference;

import static org.pdfsam.support.KeyStringValueItem.keyEmptyValue;
import static org.pdfsam.support.KeyStringValueItem.keyValue;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Named;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.IntUserPreference;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleKeyValueItem;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.LocaleKeyValueItem;
import org.pdfsam.support.io.FileType;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.Theme;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.log.MaxLogRowsChangedEvent;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.sejda.injector.Provides;

/**
 * Configuration for the PDFsam preferences components
 * 
 * @author Andrea Vacondio
 *
 */
public class PreferenceConfig {

    @Provides
    @Named("localeCombo")
    public PreferenceComboBox<LocaleKeyValueItem> localeCombo(UserContext userContext) {
        return new PreferenceComboBox<>(StringUserPreference.LOCALE, userContext);
    }

    @Provides
    @Named("themeCombo")
    public PreferenceComboBox<KeyStringValueItem<String>> themeCombo(UserContext userContext) {
        PreferenceComboBox<KeyStringValueItem<String>> themeCombo = new PreferenceComboBox<>(StringUserPreference.THEME,
                userContext);
        themeCombo.setId("themeCombo");
        themeCombo.getItems().addAll(Arrays.stream(Theme.values()).map(t -> keyValue(t.toString(), t.friendlyName()))
                .collect(Collectors.toList()));

        themeCombo.setValue(keyEmptyValue(userContext.getTheme()));
        return themeCombo;
    }

    @Provides
    @Named("startupModuleCombo")
    public PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo(List<Module> modules,
            UserContext userContext) {
        PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo = new PreferenceComboBox<>(
                StringUserPreference.STARTUP_MODULE, userContext);
        startupModuleCombo.setId("startupModuleCombo");
        startupModuleCombo.getItems().add(keyValue("", DefaultI18nContext.getInstance().i18n("Dashboard")));
        modules.stream().map(ModuleKeyValueItem::new).forEach(startupModuleCombo.getItems()::add);
        startupModuleCombo.setValue(keyEmptyValue(userContext.getStartupModule()));
        return startupModuleCombo;
    }

    @Provides
    @Named("checkForUpdates")
    public PreferenceCheckBox checkForUpdates(UserContext userContext) {
        PreferenceCheckBox checkForUpdates = new PreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().i18n("Check for updates at startup"), userContext.isCheckForUpdates(),
                userContext);
        checkForUpdates.setId("checkForUpdates");
        checkForUpdates.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                .i18n("Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().addAll(Style.WITH_HELP.css());
        checkForUpdates.getStyleClass().add("spaced-vitem");
        return checkForUpdates;
    }

    @Provides
    @Named("checkForNews")
    public PreferenceCheckBox checkForNews(UserContext userContext) {
        PreferenceCheckBox checkForNews = new PreferenceCheckBox(BooleanUserPreference.CHECK_FOR_NEWS,
                DefaultI18nContext.getInstance().i18n("Check for news at startup"), userContext.isCheckForNews(),
                userContext);
        checkForNews.setId("checkForNews");
        checkForNews.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                .i18n("Set whether the application should check for news availability on startup (restart needed)")));
        checkForNews.getStyleClass().addAll(Style.WITH_HELP.css());
        checkForNews.getStyleClass().add("spaced-vitem");
        return checkForNews;
    }

    @Provides
    @Named("playSounds")
    public PreferenceCheckBox playSounds(UserContext userContext) {
        PreferenceCheckBox playSounds = new PreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                DefaultI18nContext.getInstance().i18n("Play alert sounds"), userContext.isPlaySounds(), userContext);
        playSounds.setId("playSounds");
        playSounds.setGraphic(helpIcon(DefaultI18nContext.getInstance().i18n("Turn on or off alert sounds")));
        playSounds.getStyleClass().addAll(Style.WITH_HELP.css());
        playSounds.getStyleClass().add("spaced-vitem");
        return playSounds;
    }

    @Provides
    @Named("savePwdInWorkspace")
    public PreferenceCheckBox savePwdInWorkspace(UserContext userContext) {
        PreferenceCheckBox savePwdInWorkspace = new PreferenceCheckBox(BooleanUserPreference.SAVE_PWD_IN_WORKSPACE,
                DefaultI18nContext.getInstance().i18n("Store passwords when saving a workspace file"),
                userContext.isPlaySounds(), userContext);
        savePwdInWorkspace.setId("savePwdInWorkspace");
        savePwdInWorkspace.setGraphic(helpIcon(DefaultI18nContext.getInstance().i18n(
                "If an encrypted PDF document has been opened with a password, save the password in the workspace file")));
        savePwdInWorkspace.getStyleClass().addAll(Style.WITH_HELP.css());
        savePwdInWorkspace.getStyleClass().add("spaced-vitem");
        return savePwdInWorkspace;
    }

    @Provides
    @Named("donationNotification")
    public PreferenceCheckBox donationNotification(UserContext userContext) {
        PreferenceCheckBox donationNotification = new PreferenceCheckBox(BooleanUserPreference.DONATION_NOTIFICATION,
                DefaultI18nContext.getInstance().i18n("Show donation window"), userContext.isPlaySounds(), userContext);
        donationNotification.setId("donationNotification");
        donationNotification.setGraphic(helpIcon(DefaultI18nContext.getInstance().i18n(
                "Turn on or off the notification appearing once in a while and asking the user to support PDFsam with a donation")));
        donationNotification.getStyleClass().addAll(Style.WITH_HELP.css());
        donationNotification.getStyleClass().add("spaced-vitem");
        return donationNotification;
    }

    @Provides
    @Named("fetchPremiumModules")
    public PreferenceCheckBox fetchPremiumModules(UserContext userContext) {
        PreferenceCheckBox donationNotification = new PreferenceCheckBox(BooleanUserPreference.PREMIUM_MODULES,
                DefaultI18nContext.getInstance().i18n("Show premium features"), userContext.isPlaySounds(),
                userContext);
        donationNotification.setId("fetchPremiumModules");
        donationNotification.setGraphic(helpIcon(DefaultI18nContext.getInstance().i18n(
                "Set whether the application should fetch and show premium features description in the modules dashboard")));
        donationNotification.getStyleClass().addAll(Style.WITH_HELP.css());
        donationNotification.getStyleClass().add("spaced-vitem");
        return donationNotification;
    }

    @Provides
    @Named("smartRadio")
    public PreferenceRadioButton smartRadio(UserContext userContext) {
        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT,
                DefaultI18nContext.getInstance().i18n("Use the selected PDF document directory as output directory"),
                userContext.isUseSmartOutput(), userContext);
        smartRadio.setId("smartRadio");
        return smartRadio;
    }

    @Provides
    @Named("workingDirectory")
    public PreferenceBrowsableDirectoryField workingDirectory(UserContext userContext) {
        PreferenceBrowsableDirectoryField workingDirectory = new PreferenceBrowsableDirectoryField(
                StringUserPreference.WORKING_PATH, userContext);
        workingDirectory.getTextField().setText(userContext.getDefaultWorkingPath());
        workingDirectory.setId("workingDirectory");
        workingDirectory.getStyleClass().add("spaced-vitem");
        return workingDirectory;
    }

    @Provides
    @Named("workspace")
    public PreferenceBrowsableFileField workspace(UserContext userContext) {
        PreferenceBrowsableFileField workspace = new PreferenceBrowsableFileField(StringUserPreference.WORKSPACE_PATH,
                FileType.JSON, OpenType.OPEN, userContext);
        workspace.getTextField().setText(userContext.getDefaultWorkspacePath());
        workspace.setId("workspace");
        workspace.getStyleClass().add("spaced-vitem");
        return workspace;
    }

    @Provides
    @Named("saveWorkspaceOnExit")
    public PreferenceCheckBox saveWorkspaceOnExit(UserContext userContext) {
        PreferenceCheckBox saveWorkspaceOnExit = new PreferenceCheckBox(BooleanUserPreference.SAVE_WORKSPACE_ON_EXIT,
                DefaultI18nContext.getInstance().i18n("Save default workspace on exit"),
                userContext.isSaveWorkspaceOnExit(), userContext);
        saveWorkspaceOnExit.setId("saveWorkspaceOnExit");
        saveWorkspaceOnExit.setGraphic(helpIcon(
                DefaultI18nContext.getInstance().i18n("If a default workspace is set, save it on application exit")));
        saveWorkspaceOnExit.getStyleClass().addAll(Style.WITH_HELP.css());
        saveWorkspaceOnExit.getStyleClass().add("spaced-vitem");
        return saveWorkspaceOnExit;
    }

    @Provides
    @Named("logViewRowsNumber")
    public PreferenceIntTextField logViewRowsNumber(UserContext userContext) {
        PreferenceIntTextField logRowsNumber = new PreferenceIntTextField(IntUserPreference.LOGVIEW_ROWS_NUMBER,
                userContext, Validators.positiveInteger());
        logRowsNumber.setText(Integer.toString(userContext.getNumberOfLogRows()));
        logRowsNumber.setErrorMessage(
                DefaultI18nContext.getInstance().i18n("Maximum number of rows mast be a positive number"));
        logRowsNumber.setId("logViewRowsNumber");
        logRowsNumber.validProperty().addListener((o, oldVal, newVal) -> {
            if (newVal == ValidationState.VALID) {
                eventStudio().broadcast(new MaxLogRowsChangedEvent());
            }
        });
        return logRowsNumber;
    }

}
