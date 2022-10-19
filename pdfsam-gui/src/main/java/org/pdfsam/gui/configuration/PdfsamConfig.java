/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08 nov 2016
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
package org.pdfsam.gui.configuration;

import jakarta.inject.Named;
import javafx.application.HostServices;
import javafx.scene.image.ImageView;
import javafx.stage.Stage;
import org.pdfsam.gui.WindowStatusController;
import org.pdfsam.gui.components.MainPane;
import org.pdfsam.gui.components.dialog.ClearToolConfirmationDialogController;
import org.pdfsam.gui.components.dialog.CreateOutputDirectoryDialogController;
import org.pdfsam.gui.components.dialog.LenientTaskExecutionDialogController;
import org.pdfsam.gui.components.dialog.OpenWithDialogController;
import org.pdfsam.gui.components.dialog.OverwriteDialogController;
import org.pdfsam.gui.components.dnd.FilesDropController;
import org.pdfsam.gui.components.info.InfoStageController;
import org.pdfsam.gui.components.log.LogStage;
import org.pdfsam.gui.components.notification.NotificationsController;
import org.pdfsam.gui.io.NativeOpenFileController;
import org.pdfsam.gui.io.NativeOpenUrlController;
import org.pdfsam.gui.sound.PlaySoundController;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Prototype;
import org.pdfsam.injector.Provides;

/**
 * @author Andrea Vacondio
 */
@Components({ NativeOpenFileController.class, NativeOpenUrlController.class, WindowStatusController.class,
        PlaySoundController.class, MainPane.class, NotificationsController.class, InfoStageController.class,
        OpenWithDialogController.class, OverwriteDialogController.class, CreateOutputDirectoryDialogController.class,
        ClearToolConfirmationDialogController.class, LenientTaskExecutionDialogController.class,
        FilesDropController.class, LogStage.class })
public class PdfsamConfig {

    private final HostServices services;
    private final Stage primaryStage;

    public PdfsamConfig(HostServices services, Stage primaryStage) {
        this.services = services;
        this.primaryStage = primaryStage;
    }

    @Provides
    @Named("primaryStage")
    public Stage primaryStage() {
        return primaryStage;
    }

    @Provides
    public HostServices hostServices() {
        return services;
    }

    @Provides
    @Named("errorSound")
    public String error() {
        return this.getClass().getResource("/org/pdfsam/gui/sounds/error_sound.wav").toExternalForm();
    }

    @Provides
    @Named("okSound")
    public String ok() {
        return this.getClass().getResource("/org/pdfsam/gui/sounds/ok_sound.wav").toExternalForm();
    }

    @Provides
    @Prototype
    public ImageView payoff() {
        return new ImageView(this.getClass().getResource("/org/pdfsam/gui/images/logo.png").toExternalForm());
    }
}
