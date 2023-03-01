package org.pdfsam.core;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/09/22
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

/**
 * @author Andrea Vacondio
 */
public class ConfigurableSystemProperty {

    public static final String CHECK_FOR_UPDATES_PROP = "org.pdfsam.default.checkforupdate";
    public static final String PDF_COMPRESSION_PROP = "org.pdfsam.default.compression";
    public static final String OVERWRITE_OUTPUT_PROP = "org.pdfsam.default.output.overwrite";
    public static final String CHECK_FOR_NEWS_PROP = "org.pdfsam.default.checkfornews";
    public static final String DONATE_NOTIFICATION_PROP = "org.pdfsam.default.donate.notification";
    public static final String PLAY_SOUNDS_PROP = "org.pdfsam.default.play.sounds";
    public static final String SMART_OUTPUT_PROP = "org.pdfsam.default.smart.output";
    public static final String FETCH_PREMIUM_MODULES_PROP = "org.pdfsam.default.fetch.premium.modules";
    public static final String LOCALE_PROP = "org.pdfsam.default.locale";
    public static final String THEME_PROP = "org.pdfsam.default.theme";
    public static final String PDFSAM_DISABLE_SPLIT_OPTIMIZATION = "org.pdfsam.disable.split.optimization";
    public static final String PDFSAM_DISABLE_UI_RESTORE = "org.pdfsam.disable.ui.restore";
    public static final String PDFSAM_DISABLE_SETTINGS_DEPRECATED = "org.pdfsam.settings.panel";
    public static final String PDFSAM_DISABLE_SETTINGS = "org.pdfsam.disable.settings.panel";

    private ConfigurableSystemProperty() {
        //NOOP
    }
}
