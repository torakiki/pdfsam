/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25 nov 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.home;

import org.pdfsam.model.io.NativeOpenUrlRequest;
import org.pdfsam.model.premium.PremiumTool;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * A tile showing premium tool info
 *
 * @author Andrea Vacondio
 */
public class PremiumToolTile extends HomeTile {

    PremiumToolTile(PremiumTool tool) {
        super(tool.name(), tool.description(), tool.product().graphic(), tool.product().styleClass());
        setOnAction(e -> eventStudio().broadcast(new NativeOpenUrlRequest(tool.url())));
    }
}
