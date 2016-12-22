/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08 nov 2016
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
package org.pdfsam;

import javax.inject.Named;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.UserContext;
import org.pdfsam.sound.PlaySoundController;
import org.pdfsam.ui.MainPane;
import org.pdfsam.ui.Theme;
import org.pdfsam.ui.dialog.CreateOutputDirectoryDialogController;
import org.pdfsam.ui.dialog.OpenWithDialogController;
import org.pdfsam.ui.dialog.OverwriteDialogController;
import org.pdfsam.ui.info.InfoStageController;
import org.pdfsam.ui.notification.NotificationsController;
import org.sejda.injector.Auto;
import org.sejda.injector.Components;
import org.sejda.injector.Prototype;
import org.sejda.injector.Provides;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.image.ImageView;

/**
 * @author Andrea Vacondio
 *
 */
@Components({ OpenFileController.class, WindowStatusController.class, PlaySoundController.class, MainPane.class,
        NotificationsController.class, InfoStageController.class, OpenWithDialogController.class,
        OverwriteDialogController.class, CreateOutputDirectoryDialogController.class })
public class PdfsamConfig {
    private static final Logger LOG = LoggerFactory.getLogger(PdfsamConfig.class);

    @Provides
    @Named("errorSound")
    public String error() {
        return this.getClass().getResource("/sounds/error_sound.wav").toExternalForm();
    }

    @Provides
    @Named("okSound")
    public String ok() {
        return this.getClass().getResource("/sounds/ok_sound.wav").toExternalForm();
    }

    @Provides
    @Prototype
    public ImageView payoff() {
        return new ImageView(this.getClass().getResource("/images/payoff.png").toExternalForm());
    }

    @Provides
    @Auto
    public UserContext userContext() {
        return new DefaultUserContext();
    }

    @Provides
    public StylesConfig styles() {
        String themeString = userContext().getTheme();
        Theme selected = Theme.ROUNDISH;
        try {
            selected = Theme.valueOf(themeString);
        } catch (IllegalArgumentException e) {
            LOG.warn("Unable to find selected theme: {}.", themeString);
        }
        return new StylesConfig(selected);
    }
}
