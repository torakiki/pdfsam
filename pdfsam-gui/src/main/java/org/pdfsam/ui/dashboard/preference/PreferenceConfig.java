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

import static org.apache.commons.lang3.StringUtils.capitalize;

import java.util.Arrays;
import java.util.stream.Collectors;

import javafx.scene.control.Tooltip;

import javax.inject.Inject;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.IntUserPreference;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.Theme;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.LocaleKeyValueItem;
import org.pdfsam.support.io.FileType;
import org.pdfsam.support.validation.Validators;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration for the PDFsam preferences components
 * 
 * @author Andrea Vacondio
 *
 */
@Configuration
public class PreferenceConfig {

    private static final Integer THUMB_SIZE_LOWER = 130;
    private static final Integer THUMB_SIZE_UPPER = 390;

    @Inject
    private UserContext userContext;

    @Bean(name = "localeCombo")
    public PreferenceComboBox<LocaleKeyValueItem> localeCombo() {
        return new PreferenceComboBox<>(StringUserPreference.LOCALE, userContext);
    }

    @Bean(name = "themeCombo")
    public PreferenceComboBox<KeyStringValueItem<String>> themeCombo() {
        PreferenceComboBox<KeyStringValueItem<String>> themeCombo = new PreferenceComboBox<>(
                StringUserPreference.THEME, userContext);
        themeCombo.setId("themeCombo");
        themeCombo.getItems().addAll(
                Arrays.stream(Theme.values())
                        .map(t -> new KeyStringValueItem<>(t.toString(), capitalize(t.toString().toLowerCase())))
                        .collect(Collectors.toList()));

        themeCombo.setValue(new KeyStringValueItem<>(userContext.getTheme(), ""));
        return themeCombo;
    }

    @Bean(name = "thumbnailsCombo")
    public PreferenceComboBox<KeyStringValueItem<String>> thumbnailsCombo() {
        return new PreferenceComboBox<>(StringUserPreference.THUMBNAILS_IDENTIFIER, userContext);
    }

    @Bean(name = "checkForUpdates")
    public PreferenceCheckBox checkForUpdates() {
        PreferenceCheckBox checkForUpdates = new PreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().i18n("Check for updates at startup"), userContext.isCheckForUpdates(),
                userContext);
        checkForUpdates.setId("checkForUpdates");
        checkForUpdates.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().add("spaced-vitem");
        return checkForUpdates;
    }

    @Bean(name = "playSounds")
    public PreferenceCheckBox playSounds() {
        PreferenceCheckBox playSounds = new PreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS, DefaultI18nContext
                .getInstance().i18n("Play alert sounds"), userContext.isPlaySounds(), userContext);
        playSounds.setId("playSounds");
        playSounds.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Turn on or off alert sounds")));
        playSounds.getStyleClass().add("spaced-vitem");
        return playSounds;
    }

    @Bean(name = "askConfirmation")
    public PreferenceCheckBox askConfirmation() {
        PreferenceCheckBox askConfirmation = new PreferenceCheckBox(BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION,
                DefaultI18nContext.getInstance().i18n(
                        "Ask for confirmation when the \"Overwrite\" checkbox is selected"),
                userContext.isAskOverwriteConfirmation(), userContext);
        askConfirmation.setId("askConfirmation");
        askConfirmation.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Show a dialog box asking the user for confirmation when the \"overwrite\" is selected")));
        askConfirmation.getStyleClass().add("spaced-vitem");
        return askConfirmation;
    }

    @Bean(name = "highQualityThumbnails")
    public PreferenceCheckBox highQualityThumbnails() {
        PreferenceCheckBox highQualityThumbnails = new PreferenceCheckBox(BooleanUserPreference.HIGH_QUALITY_THUMB,
                DefaultI18nContext.getInstance().i18n("High quality thumbnails"),
                userContext.isHighQualityThumbnails(), userContext);
        highQualityThumbnails.setId("highQualityThumbnails");
        return highQualityThumbnails;
    }

    @Bean(name = "smartRadio")
    public PreferenceRadioButton smartRadio() {
        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT,
                DefaultI18nContext.getInstance().i18n("Use the selected PDF document directory as output directory"),
                userContext.isUseSmartOutput(), userContext);
        smartRadio.setId("smartRadio");
        return smartRadio;
    }

    @Bean(name = "workingDirectory")
    public PreferenceBrowsableDirectoryField workingDirectory() {
        PreferenceBrowsableDirectoryField workingDirectory = new PreferenceBrowsableDirectoryField(
                StringUserPreference.WORKING_PATH, userContext);
        workingDirectory.getTextField().setText(userContext.getDefaultWorkingPath());
        workingDirectory.setId("workingDirectory");
        return workingDirectory;
    }

    @Bean(name = "workspace")
    public PreferenceBrowsableFileField workspace() {
        PreferenceBrowsableFileField workspace = new PreferenceBrowsableFileField(StringUserPreference.WORKSPACE_PATH,
                FileType.XML, userContext);
        workspace.getTextField().setText(userContext.getDefaultWorkspacePath());
        workspace.setId("workspace");
        return workspace;
    }

    @Bean(name = "thumbnailsSize")
    public PreferenceIntTextField thumbnailsSize() {
        PreferenceIntTextField thumbnails = new PreferenceIntTextField(IntUserPreference.THUMBNAILS_SIZE, userContext,
                Validators.newIntRangeString(THUMB_SIZE_LOWER, THUMB_SIZE_UPPER));
        thumbnails.setText(Integer.toString(userContext.getThumbnailsSize()));
        thumbnails.setErrorMessage(DefaultI18nContext.getInstance().i18n("Size must be between {0}px and {1}px",
                THUMB_SIZE_LOWER.toString(), THUMB_SIZE_UPPER.toString()));
        String helpText = DefaultI18nContext.getInstance().i18n(
                "Pixel size of the thumbnails (between {0}px and {1}px)", THUMB_SIZE_LOWER.toString(),
                THUMB_SIZE_UPPER.toString());
        thumbnails.setPromptText(helpText);
        thumbnails.setTooltip(new Tooltip(helpText));
        thumbnails.setId("thumbnailsSize");
        return thumbnails;
    }

}
