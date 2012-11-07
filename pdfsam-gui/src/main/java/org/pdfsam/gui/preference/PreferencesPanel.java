/*
 * Created on 05/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.preference;

import javax.swing.GroupLayout;
import javax.swing.Icon;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.gui.AbstractContentPanel;
import org.pdfsam.support.Components;
import org.pdfsam.support.SharedJFileChooser;
import org.pdfsam.support.filter.FileFilterType;

import static javax.swing.GroupLayout.Alignment.LEADING;
import static org.pdfsam.support.Components.GAP;

/**
 * Panel showing preferences and allowing the user to set them.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PreferencesPanel extends AbstractContentPanel {

    @SuppressWarnings("serial")
    public PreferencesPanel() {

        GroupLayout layout = new GroupLayout(this);
        layout.setAutoCreateContainerGaps(true);
        this.setLayout(layout);

        JPanel language = Components.newLabeledComponent(new LocalesComboBox(), DefaultI18nContext.getInstance()
                .getI18n().tr("Language:"));
        JPanel theme = Components.newLabeledComponent(new ThemesComboBox(), DefaultI18nContext.getInstance().getI18n()
                .tr("Theme:"));

        BooleanPreferenceCheckBox checkForUpdates = new BooleanPreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().getI18n().tr("Check for updates at startup"), DefaultUserContext
                        .getInstance().isCheckForUpdates());
        checkForUpdates.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Set whether new version availability should be checked on startup (restart needed)"));

        BooleanPreferenceCheckBox playSounds = new BooleanPreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                DefaultI18nContext.getInstance().getI18n().tr("Play alert sounds"), DefaultUserContext.getInstance()
                        .isPlaySounds());
        playSounds.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n().tr("Turn on or off alert sounds"));

        BooleanPreferenceCheckBox askConfirmation = new BooleanPreferenceCheckBox(
                BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION, DefaultI18nContext.getInstance().getI18n()
                        .tr("Ask for confirmation when overwrite checkbox is selected"), DefaultUserContext
                        .getInstance().isAskOverwriteConfirmation());
        askConfirmation.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Show a dialog box asking the user for confirmation when the \"overwrite\" is selected"));

        BrowsableField defaultWorkspace = new BrowsableField(DefaultI18nContext.getInstance().getI18n()
                .tr("Load default workspace at startup:"), StringUserPreference.WORKSPACE_PATH) {
            @Override
            JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.XML, JFileChooser.FILES_AND_DIRECTORIES);
            }
        };
        defaultWorkspace.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Select a previously saved workspace that will be automatically loaded at startup"));
        defaultWorkspace.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkspacePath());

        BrowsableField defaultWorkingPath = new BrowsableField(DefaultI18nContext.getInstance().getI18n()
                .tr("Default working directory:"), StringUserPreference.WORKING_PATH) {
            @Override
            JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.DIRECTORIES, JFileChooser.DIRECTORIES_ONLY);
            }
        };
        defaultWorkingPath.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Select a directory where documents will be saved and loaded by default"));
        defaultWorkingPath.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkingPath());

        layout.setVerticalGroup(layout.createSequentialGroup().addComponent(language).addGap(GAP).addComponent(theme)
                .addGap(GAP).addComponent(checkForUpdates).addGap(GAP).addComponent(playSounds).addGap(GAP)
                .addComponent(askConfirmation).addGap(GAP).addComponent(defaultWorkspace).addGap(GAP)
                .addComponent(defaultWorkingPath));

        layout.setHorizontalGroup(layout.createSequentialGroup().addGroup(
                layout.createParallelGroup(LEADING, false).addComponent(language).addComponent(theme)
                        .addComponent(checkForUpdates).addComponent(playSounds).addComponent(askConfirmation)
                        .addComponent(defaultWorkspace).addComponent(defaultWorkingPath)));

    }

    @Override
    public String getPanelId() {
        return "Preferences";
    }

    @Override
    public String getPanelName() {
        return DefaultI18nContext.getInstance().getI18n().tr("Preferences");
    }

    @Override
    public Icon getPanelIcon() {
        // TODO Auto-generated method stub
        return null;
    }

}
