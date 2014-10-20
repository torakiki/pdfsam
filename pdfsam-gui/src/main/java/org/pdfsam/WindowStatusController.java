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

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;
import javafx.stage.Stage;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ui.SetLatestStageStatusRequest;
import org.pdfsam.ui.StageMode;
import org.pdfsam.ui.StageStatus;
import org.pdfsam.ui.StageService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller for the Window status
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class WindowStatusController {
    private static final Logger LOG = LoggerFactory.getLogger(WindowStatusController.class);
    public static final String PDFSAM_DISABLE_UI_RESTORE = "pdfsam.disable.ui.restore";

    private Stage stage;
    private StageService service;

    @Inject
    WindowStatusController(StageService service) {
        this.service = service;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
        this.stage.setOnCloseRequest(e -> {
            StageStatus status = new StageStatus(this.stage.getX(), this.stage.getY(), this.stage.getWidth(),
                    this.stage.getHeight());
            status.setMode(StageMode.valueFor(this.stage));
            eventStudio().broadcast(new SetLatestStageStatusRequest(status));
        });
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
        Rectangle2D bounds = Screen.getPrimary().getBounds();
        stage.setX(bounds.getMinX());
        stage.setY(bounds.getMinY());
        stage.setWidth(bounds.getWidth());
        stage.setHeight(bounds.getHeight());
        stage.setMaximized(true);

    }

    private void restore(StageStatus latestStatus) {
        stage.setX(latestStatus.getX());
        stage.setY(latestStatus.getY());
        stage.setWidth(latestStatus.getWidth());
        stage.setHeight(latestStatus.getHeight());
        latestStatus.getMode().restore(stage);
    }

    private boolean hasAvailableScreen(StageStatus status) {
        return !Screen.getScreensForRectangle(status.getX(), status.getY(), status.getWidth(), status.getHeight())
                .isEmpty();
    }
}
