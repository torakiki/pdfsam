/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
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

/**
 * Possible configurable properties
 * 
 * @author Andrea Vacondio
 */
public enum ConfigurableProperty {
    HOME_URL("pdfsam.home.url"),
    HOME_LABEL("pdfsam.home.label"),
    VERSION("pdfsam.version"),
    COPYRIGHT("pdfsam.copyright"),
    LICENSE_NAME("pdfsam.license.name"),
    LICENSE_URL("pdfsam.license.url"),
    FEED_URL("pdfsam.feed.url"),
    THANKS_URL("pdfsam.thanks.url"),
    TRACKER_URL("pdfsam.tracker.url"),
    SUPPORT_URL("pdfsam.support.url"),
    QUICKSTART_URL("pdfsam.quickstart.url"),
    TWITTER_URL("pdfsam.twitter.url"),
    FACEBOOK_URL("pdfsam.facebook.url"),
    GPLUS_URL("pdfsam.gplus.url"),
    TRANSLATE_URL("pdfsam.translate.url"),
    SCM_URL("pdfsam.scm.url"),
    DONATE_URL("pdfsam.donate.url"),
    TWEETER_SHARE_URL("pdfsam.tweeter.share.url"),
    FACEBOOK_SHARE_URL("pdfsam.facebook.share.url"),
    GPLUS_SHARE_URL("pdfsam.gplus.share.url"),
    NEWS_URL("pdfsam.news.url"),
    PREMIUM_MODULES_URL("pdfsam.premium.modules.url"),
    DOWNLOAD_URL("pdfsam.download.url");

    public final String prop;

    private ConfigurableProperty(String prop) {
        this.prop = prop;
    }
}
