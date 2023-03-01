package org.pdfsam.gui;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 04/10/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import javafx.stage.Stage;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.ui.SetTitleRequest;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@Auto
public class ApplicationTitleController {
    public void setStage(Stage primaryStage) {
        var applicationName = app().instance(AppBrand.class).property(BrandableProperty.SHORT_NAME, "PDFsam Basic");
        primaryStage.setTitle(applicationName);
        eventStudio().add(SetTitleRequest.class, request -> {
            if (isNotBlank(request.title())) {
                primaryStage.setTitle(applicationName + " - " + request.title());
            }
        });
    }
}
