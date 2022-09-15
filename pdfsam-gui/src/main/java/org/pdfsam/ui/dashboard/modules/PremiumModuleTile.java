/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25 nov 2016
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
package org.pdfsam.ui.dashboard.modules;

import org.pdfsam.premium.PremiumModule;
import org.pdfsam.ui.commons.NativeOpenUrlRequest;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * A tile showing premium modules info
 * 
 * @author Andrea Vacondio
 *
 */
public class PremiumModuleTile extends DashboardTile {

    PremiumModuleTile(PremiumModule module) {
        super(module.getName(), module.getDescription(), module.getProduct().graphic());
        setOnAction(e -> eventStudio().broadcast(new NativeOpenUrlRequest(module.getUrl())));
    }
}
