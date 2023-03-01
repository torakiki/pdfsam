/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
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
package org.pdfsam.gui;

import jakarta.inject.Inject;
import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;
import javafx.stage.Stage;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.service.ui.StageService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

import static org.pdfsam.core.ConfigurableSystemProperty.PDFSAM_DISABLE_UI_RESTORE;

/**
 * Controller for the Window status
 *
 * @author Andrea Vacondio
 */
@Auto
public class WindowStatusController {
    private static final Logger LOG = LoggerFactory.getLogger(WindowStatusController.class);

    private Stage stage;
    private final StageService service;

    @Inject
    WindowStatusController(StageService service) {
        this.service = service;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
        initUi();
    }

    private void initUi() {
        StageStatus latestStatus = service.getLatestStatus();
        if (!Boolean.getBoolean(PDFSAM_DISABLE_UI_RESTORE) && !StageStatus.NULL.equals(latestStatus)
                && hasAvailableScreen(latestStatus)) {
            restore(latestStatus);
            LOG.trace("Stage status restored to {}", latestStatus);
        } else {
            defaultStageStatus();
            LOG.trace("Stage status set to default values");
        }
    }

    private void defaultStageStatus() {
        Rectangle2D primScreenBounds = Screen.getPrimary().getVisualBounds();
        stage.setX((primScreenBounds.getWidth() - stage.getWidth()) / 2);
        stage.setY((primScreenBounds.getHeight() - stage.getHeight()) / 4);
        stage.setMaximized(true);
    }

    private void restore(StageStatus latestStatus) {
        stage.setX(latestStatus.x());
        stage.setY(latestStatus.y());
        stage.setWidth(latestStatus.width());
        stage.setHeight(latestStatus.height());

        if (isNotMac()) {
            latestStatus.mode().restore(stage);
        }
    }

    private boolean isNotMac() {
        return !Optional.of(System.getProperty("os.name")).orElse("").toLowerCase().contains("mac");
    }

    private boolean hasAvailableScreen(StageStatus status) {
        return !Screen.getScreensForRectangle(status.x(), status.y(), status.width(), status.height()).isEmpty();
    }
}
