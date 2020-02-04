/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
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
package org.pdfsam.ui.info;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Provider;

import org.pdfsam.injector.Auto;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.eventstudio.annotation.EventListener;

/**
 * Controller displaying the InfoStage when required
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class InfoStageController {

    private Provider<InfoStage> stageProvider;

    @Inject
    InfoStageController(Provider<InfoStage> stageProvider) {
        this.stageProvider = stageProvider;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener(priority = Integer.MAX_VALUE)
    @SuppressWarnings("unused")
    void requestShow(ShowPdfDescriptorRequest event) {
        InfoStage stage = stageProvider.get();
        if (!stage.isShowing()) {
            stage.centerOnScreen();
            stage.show();
        }
        stage.requestFocus();
    }

}
