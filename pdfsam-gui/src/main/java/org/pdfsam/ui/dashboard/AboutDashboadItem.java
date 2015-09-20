/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.dashboard;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.dashboard.about.AboutDashboardPane;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * About item for the dashboard
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class AboutDashboadItem implements DashboardItem {

    private AboutDashboardPane pane;

    @Inject
    AboutDashboadItem(AboutDashboardPane pane) {
        this.pane = pane;
    }

    public String id() {
        return "ABOUT";
    }

    public String name() {
        return DefaultI18nContext.getInstance().i18n("About");
    }

    public Pane pane() {
        return pane;
    }

    public Node graphic() {
        return GlyphsDude.createIcon(FontAwesomeIcon.INFO_CIRCLE, "26.0");
    }

    public int priority() {
        return 0;
    }
}
