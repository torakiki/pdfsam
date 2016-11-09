/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
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

import java.util.Optional;

import javax.inject.Inject;

import org.pdfsam.ui.StageService;
import org.pdfsam.ui.StageStatus;
import org.sejda.injector.Auto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;
import javafx.stage.Stage;

/**
 * Controller for the Window status
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
class WindowStatusController {
    private static final Logger LOG = LoggerFactory.getLogger(WindowStatusController.class);
    public static final String PDFSAM_DISABLE_UI_RESTORE = "org.pdfsam.disable.ui.restore";

    private Stage stage;
    private StageService service;

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
        stage.setX(latestStatus.getX());
        stage.setY(latestStatus.getY());
        stage.setWidth(latestStatus.getWidth());
        stage.setHeight(latestStatus.getHeight());

        if (isNotMac()) {
            latestStatus.getMode().restore(stage);
        }
    }

    private boolean isNotMac() {
        return !Optional.of(System.getProperty("os.name")).orElse("").toLowerCase().contains("mac");
    }

    private boolean hasAvailableScreen(StageStatus status) {
        return !Screen.getScreensForRectangle(status.getX(), status.getY(), status.getWidth(), status.getHeight())
                .isEmpty();
    }
}
